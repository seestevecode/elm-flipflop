module Main exposing (main)

import Board exposing (Board)
import Browser
import Card exposing (Card)
import Constants as Const
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import GameType exposing (GameType)
import Html exposing (Html)
import List.Extra as ListX
import Random
import Random.List


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
    }


type Selection
    = NoSelection
    | Spare Card
    | Tableau (List Card) Int


type GameState
    = NewGame
    | Playing
    | GameOver


type Msg
    = NewDeck (List Card)
    | Undo
    | Restart
    | StartGame GameType
    | SelectMsg SelectMsg
    | MoveMsg Board.MoveMsg


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
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDeck cards ->
            ( { model | board = GameType.boardFromDeck model.gameType cards }
            , Cmd.none
            )

        Undo ->
            case model.undoHistory of
                [] ->
                    ( model, Cmd.none )

                last :: rest ->
                    ( { model
                        | board = last
                        , selection = NoSelection
                        , undoHistory = rest
                        , undoUsed = True
                      }
                    , Cmd.none
                    )

        Restart ->
            case List.reverse model.undoHistory of
                [] ->
                    ( model, Cmd.none )

                first :: _ ->
                    ( { model
                        | board = first
                        , selection = NoSelection
                        , undoHistory = []
                        , undoUsed = True
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
            updateSelect subMsg model

        MoveMsg subMsg ->
            updateMove subMsg model


updateSelect : SelectMsg -> Model -> ( Model, Cmd Msg )
updateSelect msg model =
    case msg of
        ClearSelection ->
            ( { model | selection = NoSelection }, Cmd.none )

        SelectTableau card ->
            ( { model
                | selection =
                    let
                        cards =
                            Board.selectFromCardInTableau
                                card
                                model.board.tableau
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
              }
            , Cmd.none
            )

        SelectSpare card ->
            ( { model
                | selection =
                    if model.selection == Spare card then
                        NoSelection

                    else
                        Spare card
              }
            , Cmd.none
            )


updateMove : Board.MoveMsg -> Model -> ( Model, Cmd Msg )
updateMove msg model =
    case msg of
        Board.MoveTableauToTableau cards fromCol toCol ->
            ( updateModelTabToTab model cards fromCol toCol, Cmd.none )

        Board.MoveSpareToTableau card toCol ->
            if Board.validSprToTab model.board card toCol then
                ( { model | board = Board.moveSprToTab model.board card toCol }
                    |> updateModelAfterMove 1
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Board.MoveSpareToFoundation card toFnd ->
            if Board.validSprToFnd model.board card toFnd then
                ( { model | board = Board.moveSprToFnd model.board card toFnd }
                    |> updateModelAfterMove 1
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Board.MoveTableauToFoundation cards fromTab toFnd ->
            if Board.validTabToFnd model.board cards fromTab toFnd then
                ( { model
                    | board = Board.moveTabToFnd model.board cards fromTab toFnd
                  }
                    |> updateModelAfterMove (List.length cards)
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Board.MoveStockToTableau ->
            ( { model | board = Board.addCardsFromStock model.board }
                |> updateModelAfterMove 1
            , Cmd.none
            )


updateModelTabToTab : Model -> List Card -> Int -> Int -> Model
updateModelTabToTab model cards fromCol toCol =
    case Board.validTabToTab model.board cards fromCol toCol of
        ( True, Just False ) ->
            { model
                | board =
                    Board.moveTabToTab model.board cards fromCol toCol
            }
                |> updateModelAfterMove 1

        ( True, Just True ) ->
            { model
                | board =
                    Board.moveTabToTab model.board
                        (List.reverse cards)
                        fromCol
                        toCol
            }
                |> updateModelAfterMove (List.length cards)

        _ ->
            model


updateModelAfterMove : Int -> Model -> Model
updateModelAfterMove movesIncrement model =
    { model
        | selection = NoSelection
        , moves = model.moves + movesIncrement
        , undoHistory = model.board :: model.undoHistory
    }


updateGameState : Model -> Model
updateGameState model =
    case model.gameState of
        NewGame ->
            { model | gameState = Playing }

        _ ->
            model


view : Model -> Html Msg
view model =
    layout
        [ padding 10, Background.color <| Const.backgroundColour ]
    <|
        row [ spacing 25, height fill ]
            [ viewMain model
            , viewSidebar model
            ]


viewMain : Model -> Element Msg
viewMain model =
    column [ spacing 25, alignTop, width <| cardWidth 6 ] <|
        case model.gameState of
            NewGame ->
                [ none ]

            Playing ->
                [ viewFoundations model, viewTableau model ]

            GameOver ->
                [ none ]


cardWidth : Float -> Length
cardWidth cards =
    px <| floor <| 68 * cards


viewFoundations : Model -> Element Msg
viewFoundations model =
    let
        spacer =
            case model.gameType.numFoundations of
                4 ->
                    [ el globalCardAtts none ]

                _ ->
                    [ none ]

        foundations =
            List.indexedMap (viewFoundation model) model.board.foundations
    in
    row [ spacing 10, centerX ] <| foundations ++ spacer


globalCardAtts : List (Attribute msg)
globalCardAtts =
    [ Border.rounded 4
    , width (cardWidth 1)
    , height (cardHeight 1)
    ]


viewFoundation : Model -> Int -> List Card -> Element Msg
viewFoundation model foundation cards =
    case List.reverse cards of
        [] ->
            viewCardSpace <|
                foundationSelectionAtts model.selection foundation

        last :: _ ->
            viewCard model last <|
                foundationSelectionAtts model.selection foundation


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


viewCardSpace : List (Attribute Msg) -> Element Msg
viewCardSpace atts =
    el
        (globalCardAtts
            ++ atts
            ++ [ Background.color <| Const.cardSpaceBackground ]
        )
    <|
        none


viewCard : Model -> Card -> List (Attribute Msg) -> Element Msg
viewCard model card attr =
    case card.orientation of
        Card.FaceUp ->
            viewCardFaceup model card attr

        Card.FaceDown ->
            viewCardFacedown


viewCardFaceup : Model -> Card -> List (Attribute Msg) -> Element Msg
viewCardFaceup model card attr =
    column
        (globalCardAtts
            ++ attr
            ++ [ Background.color <| rgb 1 1 1 ]
        )
        [ viewCardFaceupHead model card, Card.viewCardFaceupBody card ]


viewCardFaceupHead : Model -> Card -> Element msg
viewCardFaceupHead model card =
    row
        [ padding 3
        , width fill
        , Font.size 20
        , spacing 3
        , Border.roundEach
            { topLeft = 4
            , topRight = 4
            , bottomLeft = 0
            , bottomRight = 0
            }
        , Background.color <| Tuple.second <| Card.suitOutput card.suit
        , Font.color <| rgb 1 1 1
        ]
        [ Card.viewRank card.rank
        , el [] <|
            text <|
                Tuple.first <|
                    Card.suitOutput card.suit
        , if cardSelected model.selection card then
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


viewSidebar : Model -> Element Msg
viewSidebar model =
    let
        sidebarHeader =
            el [ centerX, Font.size 22, Font.bold ] <|
                text "FlipFlop"

        sidebarBurger =
            Input.button [ centerX, Font.size 25 ]
                { onPress = Nothing, label = text "≡" }
    in
    column sidebarAtts <|
        case model.gameState of
            NewGame ->
                [ sidebarHeader
                , el [] <| text "New Game"
                , sidebarBurger
                , viewSelectGame
                ]

            Playing ->
                [ sidebarHeader
                , viewInfo model
                , sidebarBurger
                , viewSpare model
                , viewStock model
                ]

            GameOver ->
                [ sidebarHeader
                , el [] <| text "Game Over"
                , sidebarBurger
                , viewSelectGame
                ]


sidebarAtts : List (Attribute Msg)
sidebarAtts =
    [ spacing 25
    , alignTop
    , width <| cardWidth 2.5
    , height fill
    , padding 10
    , Background.color <| rgba 0 0 0 0.25
    , Font.size 15
    , Font.color <| rgb 1 1 1
    ]


viewSelectGame : Element Msg
viewSelectGame =
    let
        newGameLink gameType =
            Input.button [ centerX ]
                { onPress = Just (StartGame <| GameType.getGameType gameType)
                , label = text <| .name <| GameType.getGameType gameType
                }
    in
    column [ spacing 20, centerX ] <|
        List.map newGameLink (Dict.keys GameType.validGameTypes)


viewInfo : Model -> Element Msg
viewInfo model =
    let
        numFoundationCards =
            model.board.foundations |> List.concat |> List.length |> toFloat

        numTargetCards =
            model.gameType.numFoundations * 13 |> toFloat
    in
    column
        [ Font.size 15
        , spacing 10
        , Font.color <| rgb 1 1 1
        , centerX
        , height <| cardHeight 1.25
        ]
        [ el [ centerX, Font.size 18, Font.bold ] <|
            text <|
                model.gameType.name
        , viewProgress <| numFoundationCards / numTargetCards
        , el [ centerX ] <|
            text <|
                case model.moves of
                    1 ->
                        "1 move"

                    n ->
                        String.fromInt n ++ " moves"
        , el [ centerX, height (fill |> minimum 20) ] <|
            if model.undoUsed then
                text "Undo used"

            else
                none
        ]


viewProgress : Float -> Element Msg
viewProgress progress =
    let
        intProgress =
            round (progress * 100)
    in
    el [ centerX ] <| text <| String.fromInt intProgress ++ "% completed"


viewUndoButton : Element Msg
viewUndoButton =
    Input.button [ alignLeft ]
        { onPress = Just Undo, label = text "Undo" }


viewRestartButton : Element Msg
viewRestartButton =
    Input.button [ alignRight ]
        { onPress = Just Restart, label = text "Restart" }


viewTableau : Model -> Element Msg
viewTableau model =
    row [ spacing 10, centerX ] <|
        List.map
            (viewTableauColumn model)
            (Dict.keys model.board.tableau)


viewTableauColumn : Model -> Int -> Element Msg
viewTableauColumn model colIndex =
    Board.getTableauColumn model.board.tableau colIndex
        |> viewColumn model colIndex


viewColumn : Model -> Int -> List Card -> Element Msg
viewColumn model colIndex cards =
    column
        ([ alignTop, spacing -81 ]
            ++ columnSelectionAtts model.selection colIndex
            ++ columnWarningAtts cards
        )
    <|
        case cards of
            [] ->
                [ viewCardSpace [] ]

            cs ->
                let
                    viewTabCard c =
                        viewCard model
                            c
                            [ pointer
                            , Events.onClick <| SelectMsg (SelectTableau c)
                            ]
                in
                List.map viewTabCard cs


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


columnWarningAtts : List Card -> List (Attribute msg)
columnWarningAtts cards =
    if List.length cards >= 20 then
        [ Border.color Const.columnWarningColour
        , Border.widthEach { bottom = 5, top = 0, right = 0, left = 0 }
        , Border.solid
        ]

    else
        []


viewSpare : Model -> Element Msg
viewSpare model =
    let
        viewSingleSpare spare =
            case spare of
                Nothing ->
                    el globalCardAtts none

                Just s ->
                    viewCard model
                        s
                        [ Events.onClick <| SelectMsg (SelectSpare s), pointer ]

        selectAttr =
            [ Events.onClick SelectSpare, pointer ]
    in
    row [ spacing 10 ]
        [ viewSingleSpare <| Tuple.first model.board.spare
        , viewSingleSpare <| Tuple.second model.board.spare
        ]


viewStock : Model -> Element Msg
viewStock model =
    let
        stockSize =
            List.length model.board.stock
    in
    el [ Events.onClick (MoveMsg Board.MoveStockToTableau), pointer ] <|
        viewStockRow <|
            List.repeat stockSize viewCardFacedown


viewStockRow : List (Element Msg) -> Element Msg
viewStockRow els =
    el [] <|
        case els of
            [] ->
                none

            first :: rest ->
                el [ inFront <| viewStockRow rest, moveRight 15 ] <| first


viewCardFacedown : Element Msg
viewCardFacedown =
    let
        innerScale =
            1.05
    in
    el
        (globalCardAtts ++ [ Background.color <| rgb 1 1 1 ])
    <|
        el
            [ Border.rounded <| floor (4 / innerScale)
            , width <| px <| floor (68 / innerScale)
            , height <| px <| floor (105 / innerScale)
            , Background.color <| rgb255 44 49 64
            , centerX
            , centerY
            ]
        <|
            none


cardHeight : Float -> Length
cardHeight cards =
    px <| floor (105 * cards)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
