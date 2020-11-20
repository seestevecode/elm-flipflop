module Main exposing (main)

import Board exposing (Board)
import Browser
import Browser.Events
import Card exposing (Card)
import Constants as Const
import Dict exposing (keys)
import Element
    exposing
        ( Attribute
        , Element
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , column
        , el
        , fill
        , height
        , layout
        , newTabLink
        , none
        , padding
        , paddingEach
        , paragraph
        , pointer
        , px
        , rgb
        , rgba
        , row
        , spacing
        , text
        , textColumn
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import GameType exposing (GameType)
import Html exposing (Html)
import Random
import Random.List
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel <| GameType.getGameType 0, Cmd.none )


type alias Model =
    { gameType : GameType
    , board : Board
    , selection : Selection
    , moves : Int
    , undoHistory : List Board
    , undoUsed : Bool
    , gameState : GameState
    , durationSeconds : Int
    }


type Selection
    = NoSelection
    | Spare Card
    | Tableau (List Card) Int


type GameState
    = NewGame
    | Playing
    | GameOver
    | Paused


type Msg
    = NewDeck (List Card)
    | Undo
    | Restart
    | TogglePause
    | StartGame GameType
    | SelectMsg SelectMsg
    | MoveMsg Board.MoveMsg
    | IncrementTimer


type SelectMsg
    = ClearSelection
    | SelectSpare Card
    | SelectTableau Card


initModel : GameType -> Model
initModel gameType =
    { gameType = gameType
    , board = GameType.boardFromDeck gameType <| GameType.deck gameType
    , selection = NoSelection
    , moves = 0
    , undoHistory = []
    , undoUsed = False
    , gameState = NewGame
    , durationSeconds = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDeck cards ->
            ( { model | board = GameType.boardFromDeck model.gameType cards }
            , Cmd.none
            )

        Undo ->
            ( updateUndo model, Cmd.none )

        Restart ->
            ( updateRestart model, Cmd.none )

        TogglePause ->
            ( { model
                | gameState =
                    case model.gameState of
                        Paused ->
                            Playing

                        Playing ->
                            Paused

                        _ ->
                            model.gameState
              }
            , Cmd.none
            )

        StartGame newGameType ->
            ( initModel newGameType |> updateGameState
            , Random.generate NewDeck <|
                Random.List.shuffle <|
                    GameType.deck newGameType
            )

        SelectMsg subMsg ->
            ( { model | selection = updateSelect subMsg model }, Cmd.none )

        MoveMsg subMsg ->
            ( updateMove subMsg model, Cmd.none )

        IncrementTimer ->
            ( { model | durationSeconds = model.durationSeconds + 1 }, Cmd.none )


updateHistory : Model -> Model
updateHistory model =
    { model | undoHistory = model.board :: model.undoHistory }


updateBoard : Board -> Model -> Model
updateBoard board model =
    { model | board = board }


updateUndo : Model -> Model
updateUndo model =
    case model.undoHistory of
        [] ->
            model

        last :: rest ->
            { model
                | board = last
                , selection = NoSelection
                , undoHistory = rest
                , undoUsed = True
            }


updateRestart : Model -> Model
updateRestart model =
    case List.reverse model.undoHistory of
        [] ->
            { model | gameState = Playing }

        first :: _ ->
            { model
                | board = first
                , selection = NoSelection
                , undoHistory = []
                , undoUsed = True
                , gameState = Playing
            }


updateSelect : SelectMsg -> Model -> Selection
updateSelect msg model =
    case msg of
        ClearSelection ->
            NoSelection

        SelectTableau card ->
            let
                cards =
                    Board.selectFromCardInTableau card model.board.tableau
            in
            case Board.tableauColumn model.board.tableau card of
                Just col ->
                    if
                        Card.selectionValidTableauMove cards
                            || Card.selectionValidFoundationMove cards
                    then
                        Tableau cards col

                    else
                        model.selection

                Nothing ->
                    model.selection

        SelectSpare card ->
            if model.selection == Spare card then
                NoSelection

            else
                Spare card


updateMove : Board.MoveMsg -> Model -> Model
updateMove msg model =
    case msg of
        Board.MoveTableauToTableau cards fromCol toCol ->
            updateModelTabToTab model cards fromCol toCol

        Board.MoveSpareToTableau card toCol ->
            updateMoveIfCheck model
                (Board.validSprToTab model.board card toCol)
                (Board.moveSprToTab model.board card toCol)
                1

        Board.MoveSpareToFoundation card toFnd ->
            updateMoveIfCheck model
                (Board.validSprToFnd model.board card toFnd)
                (Board.moveSprToFnd model.board card toFnd)
                1

        Board.MoveTableauToFoundation cards fromTab toFnd ->
            updateMoveIfCheck model
                (Board.validTabToFnd model.board cards toFnd)
                (Board.moveTabToFnd model.board cards fromTab toFnd)
                (List.length cards)

        Board.MoveStockToTableau ->
            updateMoveIfCheck model True (Board.addCardsFromStock model.board) 1


updateMoveIfCheck : Model -> Bool -> Board -> Int -> Model
updateMoveIfCheck model check board moveIncrement =
    if check then
        model
            |> updateHistory
            |> updateBoard board
            |> updateModelAfterMove moveIncrement

    else
        model


updateModelTabToTab : Model -> List Card -> Int -> Int -> Model
updateModelTabToTab model cards fromCol toCol =
    case Board.validTabToTab model.board cards toCol of
        ( True, Just False ) ->
            updateMoveIfCheck model
                True
                (Board.moveTabToTab model.board cards fromCol toCol)
                1

        ( True, Just True ) ->
            updateMoveIfCheck model
                True
                (Board.moveTabToTab model.board
                    (List.reverse cards)
                    fromCol
                    toCol
                )
                (List.length cards)

        _ ->
            model


updateModelAfterMove : Int -> Model -> Model
updateModelAfterMove movesIncrement model =
    { model
        | selection = NoSelection
        , moves = model.moves + movesIncrement
    }
        |> updateGameState


updateGameState : Model -> Model
updateGameState model =
    case model.gameState of
        NewGame ->
            { model | gameState = Playing }

        Playing ->
            { model
                | gameState =
                    if progress model == 100 then
                        GameOver

                    else
                        Playing
            }

        _ ->
            model


view : Model -> Html Msg
view model =
    layout [ padding 10 ] <|
        row [ centerX, spacing 25, height fill ]
            [ viewMain model, viewSidebar model ]


viewMain : Model -> Element Msg
viewMain model =
    column [ spacing 25, alignTop, width <| px <| Const.cardWidth * 6 ] <|
        case model.gameState of
            NewGame ->
                [ viewDummyFoundations, viewInstructions ]

            Playing ->
                [ viewFoundations model, viewTableau model ]

            GameOver ->
                [ viewFoundations model, viewSummary model ]

            Paused ->
                [ viewFoundations model, viewInstructions ]


viewSummary : Model -> Element msg
viewSummary model =
    column
        [ centerX
        , spacing 20
        , padding 50
        , Font.color <| rgb 1 1 1
        , width fill
        , Background.color Const.cardSpaceColour
        ]
        [ el [ centerX, Font.bold, Font.size 30 ] <| text <| model.gameType.name
        , el [ centerX ] <|
            text <|
                "Time: "
                    ++ secondsToMinSec model.durationSeconds
        , el [ centerX ] <| text <| "Moves: " ++ String.fromInt model.moves
        ]


viewDummyFoundations : Element Msg
viewDummyFoundations =
    row [ spacing 10 ] <|
        List.map (\card -> viewCard NoSelection card [])
            [ Card Card.King Card.Spades Card.FaceUp 1
            , Card Card.King Card.Hearts Card.FaceUp 2
            , Card Card.King Card.Clubs Card.FaceUp 3
            , Card Card.King Card.Diamonds Card.FaceUp 4
            , Card Card.King Card.Stars Card.FaceUp 5
            ]


viewInstructions : Element msg
viewInstructions =
    textColumn
        [ width fill
        , height fill
        , padding 10
        , spacing 10
        , Background.color <| Const.cardSpaceColour
        ]
    <|
        List.map
            (\string ->
                paragraph [ Font.size 18, Font.color <| rgb 1 1 1 ]
                    [ text string ]
            )
            instructionList


instructionList : List String
instructionList =
    [ "FlipFlop Solitaire is like other solitaires, but with a few tweaks."
    , "To win, you must move all the cards in the deck to the foundations, in order."
    , "Each foundation takes an Ace, then a Two, Three, etc. all the way to a King."
    , "(If you have a card that can go up there, it's usually a good idea to move it up.)"
    , "In addition to the cards above, you have a few extra cards, found in the sidebar."
    , "A card may be moved onto any other card that is just above or below it in sequence."
    , "Nothing is lower than an Ace or higher than a King."
    , "Any stack of sequential adjacent cards can be moved together."
    , "However, you can only move a stack of cards if they all share the same suit."
    ]


viewFoundations : Model -> Element Msg
viewFoundations model =
    let
        spacer =
            case model.gameType.numFoundations of
                4 ->
                    [ el Card.globalCardAtts none ]

                _ ->
                    [ none ]

        foundations =
            List.indexedMap (viewFoundation model.selection)
                model.board.foundations
    in
    row [ spacing 10, centerX ] <| foundations ++ spacer


viewFoundation : Selection -> Int -> List Card -> Element Msg
viewFoundation selection foundation cards =
    case List.reverse cards of
        [] ->
            Card.viewCardSpace <| foundationSelectionAtts selection foundation

        last :: _ ->
            viewCard selection last <|
                foundationSelectionAtts selection foundation


foundationSelectionAtts : Selection -> Int -> List (Attribute Msg)
foundationSelectionAtts selection foundation =
    case selection of
        Spare spareCard ->
            [ pointer
            , Events.onClick <|
                MoveMsg (Board.MoveSpareToFoundation spareCard foundation)
            ]

        Tableau tabCards tabCol ->
            [ pointer
            , Events.onClick <|
                MoveMsg
                    (Board.MoveTableauToFoundation tabCards tabCol foundation)
            ]

        _ ->
            []


viewCard : Selection -> Card -> List (Attribute Msg) -> Element Msg
viewCard selection card attr =
    case card.orientation of
        Card.FaceUp ->
            viewCardFaceup selection card attr

        Card.FaceDown ->
            Card.viewCardFacedown


viewCardFaceup : Selection -> Card -> List (Attribute Msg) -> Element Msg
viewCardFaceup selection card attr =
    column
        (Card.globalCardAtts ++ attr ++ [ Background.color <| rgb 1 1 1 ])
        [ viewCardFaceupHead selection card, Card.viewCardFaceupBody card ]


viewCardFaceupHead : Selection -> Card -> Element msg
viewCardFaceupHead selection card =
    row
        [ padding 3
        , width fill
        , Font.size 20
        , spacing 3
        , Border.roundEach
            { topLeft = Const.cardCornerRound
            , topRight = Const.cardCornerRound
            , bottomLeft = 0
            , bottomRight = 0
            }
        , Background.color <| Tuple.second <| Card.suitOutput card.suit
        , Font.color <| rgb 1 1 1
        ]
        [ Card.viewRank card.rank
        , el [] <| text <| Tuple.first <| Card.suitOutput card.suit
        , if cardSelected selection card then
            el [ alignRight, Font.color <| rgb 1 1 1 ] <| text "●"

          else
            none
        ]


cardSelected : Selection -> Card -> Bool
cardSelected selection card =
    case selection of
        Spare spareCard ->
            card == spareCard

        Tableau tabCards _ ->
            List.member card tabCards

        NoSelection ->
            False



-- Sidebar


viewSidebar : Model -> Element Msg
viewSidebar model =
    column sidebarAtts <|
        case model.gameState of
            Playing ->
                [ viewHeader
                , viewGameType model.gameType
                , viewTimer model.durationSeconds
                , viewStats model
                    |> divider
                , viewSpare model
                , viewStock (List.length model.board.stock) model.gameType
                    |> divider
                , row [ width fill ]
                    [ el [ alignLeft ] <| viewUndoButton
                    , el [ alignRight ] <| viewHintButton
                    ]
                , viewPauseToggle model
                    |> divider
                , viewCredits
                ]

            Paused ->
                [ viewHeader
                , viewGameType model.gameType
                , viewTimer model.durationSeconds
                    |> divider
                , viewSelectGame
                    |> divider
                , viewRestartButton
                , viewPauseToggle model
                    |> divider
                , viewCredits
                ]

            _ ->
                [ viewHeader
                , viewIntro |> divider
                , viewSelectGame
                    |> divider
                , viewCredits
                ]


viewTimer : Int -> Element msg
viewTimer timerSeconds =
    el [ centerX, Font.size 20 ] <| text <| secondsToMinSec timerSeconds


secondsToMinSec : Int -> String
secondsToMinSec s =
    let
        minutes =
            s // 60

        seconds =
            s - (minutes * 60)
    in
    String.fromInt minutes
        ++ ":"
        ++ (String.pad 2 '0' <| String.fromInt seconds)


viewCredits : Element msg
viewCredits =
    paragraph [ alignBottom, Font.size 12, Font.center ]
        [ text "© "
        , newTabLink []
            { url = "https://seestevecode.me.uk"
            , label = text "seestevecode"
            }
        , text " - "
        , newTabLink []
            { url = "https://github.com/seestevecode/flipflop-solitaire"
            , label = text "source"
            }
        ]


viewHeader : Element msg
viewHeader =
    el [ centerX, Font.size 22, Font.bold ] <| text "FlipFlop Solitaire"


viewGameType : GameType -> Element msg
viewGameType gameType =
    column [ centerX ]
        [ el [ centerX, Font.size 18 ] <| text gameType.name
        , el [ centerX, Font.size 22 ] <| text gameType.icons
        ]


viewIntro : Element msg
viewIntro =
    textColumn [ width fill, centerX ]
        [ paragraph [ Font.center ] [ text "Based on a game" ]
        , paragraph [ Font.center ] [ text "by Zach Gage." ]
        ]


divider : Element msg -> Element msg
divider element =
    el
        [ Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color <| rgb 1 1 1
        , paddingEach { bottom = 25, top = 0, left = 0, right = 0 }
        , width fill
        ]
    <|
        element


sidebarAtts : List (Attribute Msg)
sidebarAtts =
    [ spacing 25
    , alignTop
    , width <| px 200
    , height fill
    , padding 10
    , centerX
    , Background.color <| rgba 0 0 0 0.25
    , Font.size 15
    , Font.color Const.sidebarFontColour
    ]


viewSelectGame : Element Msg
viewSelectGame =
    let
        gameTypeLabel index =
            column [ spacing 2 ]
                [ el [ centerX ] <| text <| .name <| GameType.getGameType index
                , el [ centerX, Font.size 25 ] <|
                    text <|
                        .icons <|
                            GameType.getGameType index
                ]

        newGameLink gameType =
            Input.button [ centerX ]
                { onPress = Just (StartGame <| GameType.getGameType gameType)
                , label = gameTypeLabel gameType
                }
    in
    column [ spacing 20, centerX ] <|
        [ paragraph [ centerX, Font.center ]
            [ text "Start a new game:" ]
        ]
            ++ List.map newGameLink (Dict.keys GameType.validGameTypes)


viewStats : Model -> Element Msg
viewStats model =
    let
        movesText =
            if model.moves == 1 then
                "1 move"

            else
                String.fromInt model.moves ++ " moves"

        undoTextEl =
            if model.undoUsed then
                text "Undo used"

            else
                text "Undo not used"
    in
    column
        [ Font.size 15, spacing 20, centerX ]
        [ el [ centerX ] <|
            text <|
                String.fromInt (progress model)
                    ++ "% completed"
        , el [ centerX ] <| text <| movesText
        , el [ centerX ] <| undoTextEl
        ]


progress : Model -> Int
progress model =
    let
        numFoundationCards =
            model.board.foundations |> List.concat |> List.length |> toFloat

        numTargetCards =
            model.gameType.numFoundations * 13 |> toFloat
    in
    round <| numFoundationCards / numTargetCards * 100


viewUndoButton : Element Msg
viewUndoButton =
    Input.button [ centerX ] { onPress = Just Undo, label = text "Undo" }


viewHintButton : Element Msg
viewHintButton =
    el [ centerX ] <| text "(Hint)"


viewPauseToggle : Model -> Element Msg
viewPauseToggle model =
    Input.button [ centerX ]
        { onPress = Just TogglePause
        , label =
            text <|
                case model.gameState of
                    Playing ->
                        "Pause"

                    Paused ->
                        "Resume"

                    _ ->
                        "NULL"
        }


viewRestartButton : Element Msg
viewRestartButton =
    Input.button [ centerX ] { onPress = Just Restart, label = text "Restart" }


viewTableau : Model -> Element Msg
viewTableau model =
    row [ spacing 10, centerX ] <|
        List.map (viewTableauColumn model) (Dict.keys model.board.tableau)


viewTableauColumn : Model -> Int -> Element Msg
viewTableauColumn model colIndex =
    Board.getTableauColumn model.board.tableau colIndex
        |> viewColumn model.selection colIndex


viewColumn : Selection -> Int -> List Card -> Element Msg
viewColumn selection colIndex cards =
    let
        viewCardInColumn card =
            viewCard selection
                card
                [ pointer, Events.onClick <| SelectMsg (SelectTableau card) ]
    in
    column
        ([ alignTop, spacing -81 ]
            ++ columnSelectionAtts selection colIndex
            ++ Board.columnWarningAtts cards
        )
    <|
        case cards of
            [] ->
                List.singleton <| Card.viewCardSpace []

            cs ->
                List.map viewCardInColumn cs


columnSelectionAtts : Selection -> Int -> List (Attribute Msg)
columnSelectionAtts selection colIndex =
    case selection of
        Tableau tabCards tabCol ->
            [ pointer
            , if colIndex == tabCol then
                Events.onClick (SelectMsg ClearSelection)

              else
                Events.onClick <|
                    MoveMsg
                        (Board.MoveTableauToTableau tabCards tabCol colIndex)
            ]

        Spare spareCard ->
            [ pointer
            , Events.onClick <|
                MoveMsg (Board.MoveSpareToTableau spareCard colIndex)
            ]

        _ ->
            []


viewSpare : Model -> Element Msg
viewSpare model =
    let
        viewSingleSpare spare =
            case spare of
                Nothing ->
                    el Card.globalCardAtts none

                Just s ->
                    viewCard model.selection
                        s
                        [ Events.onClick <| SelectMsg (SelectSpare s), pointer ]
    in
    row [ spacing 10, centerX ]
        [ viewSingleSpare <| Tuple.first model.board.spare
        , viewSingleSpare <| Tuple.second model.board.spare
        ]


viewStock : Int -> GameType -> Element Msg
viewStock currentGroups gameType =
    let
        initStockGroups =
            initModel gameType |> .board |> .stock |> List.length

        stockWidth =
            initStockGroups * Const.cardWidth - 50 * (initStockGroups - 1)
    in
    case currentGroups of
        0 ->
            el Card.globalCardAtts none

        numGroups ->
            el [ pointer, Events.onClick <| MoveMsg Board.MoveStockToTableau ]
                Card.viewCardFacedown
                :: List.repeat (numGroups - 1) Card.viewCardFacedown
                |> List.reverse
                |> row [ width <| px <| stockWidth, spacing -50, alignLeft ]
                |> el [ centerX ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subTimer =
            case model.gameState of
                Playing ->
                    Time.every 1000 (\_ -> IncrementTimer)

                _ ->
                    Sub.none

        subVisibility =
            Browser.Events.onVisibilityChange (\_ -> TogglePause)
    in
    Sub.batch [ subTimer, subVisibility ]
