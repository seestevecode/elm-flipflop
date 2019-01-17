module Main exposing (main)

import Board exposing (Board)
import Browser
import Card exposing (Card)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra as ListX
import Random
import Random.List


scale : Float
scale =
    1


showDebug : Bool
showDebug =
    True


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel <| getGameType 0, Cmd.none )


type alias Model =
    { gameType : GameType
    , board : Board
    , selection : Selection
    , moves : Int
    , undoHistory : List Board
    , undoUsed : Bool
    , gameState : GameState
    }


type alias GameType =
    { name : String
    , numFoundations : Int
    , numSuits : Int
    , numTableauCards : Int
    , tableauColSizes : List Int
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
    , board = boardFromDeck gameType <| deck gameType
    , selection = NoSelection
    , moves = 0
    , undoHistory = []
    , undoUsed = False
    , gameState = NewGame
    }


boardFromDeck : GameType -> List Card -> Board
boardFromDeck gameType cards =
    { foundations = List.repeat gameType.numFoundations []
    , tableau =
        cards
            |> List.take gameType.numTableauCards
            |> buildTableau gameType
    , spare =
        let
            spareCards =
                cards
                    |> List.drop gameType.numTableauCards
                    |> List.take 2
                    |> List.map Card.turnUp
        in
        ( List.head spareCards, ListX.last spareCards )
    , stock =
        cards
            |> List.drop (gameType.numTableauCards + 2)
            |> ListX.groupsOf 5
    }


buildTableau : GameType -> List Card -> Board.Tableau
buildTableau gameType cards =
    let
        tableauIndices =
            List.range 0 4

        tableauColSizes =
            gameType.tableauColSizes

        tableauColumns =
            ListX.groupsOfVarying tableauColSizes cards
    in
    List.map2 Tuple.pair tableauIndices tableauColumns
        |> Dict.fromList
        |> Board.turnUpEndCards


deck : GameType -> List Card
deck gameType =
    let
        ranks =
            List.repeat gameType.numFoundations Card.orderedRanks |> List.concat

        suits =
            ListX.cycle
                gameType.numFoundations
                (List.take gameType.numSuits Card.orderedSuits)
                |> List.repeat 13
                |> List.concat
                |> ListX.gatherEquals
                |> List.concatMap (\( c, cs ) -> c :: cs)

        allFaceDown =
            List.repeat (gameType.numFoundations * 13) Card.FaceDown

        ids =
            List.range 1 (gameType.numFoundations * 13)
    in
    List.map4 Card ranks suits allFaceDown ids


getGameType : Int -> GameType
getGameType gameTypeIndex =
    Dict.get gameTypeIndex validGameTypes
        |> Maybe.withDefault (GameType "1-suit" 4 1 25 [ 5, 5, 5, 5, 5 ])


validGameTypes : Dict Int GameType
validGameTypes =
    let
        gameTypes =
            [ GameType "1-suit" 4 1 25 [ 5, 5, 5, 5, 5 ]
            , GameType "2-suit" 4 2 25 [ 5, 5, 5, 5, 5 ]
            , GameType "3-suit" 4 3 25 [ 5, 5, 5, 5, 5 ]
            , GameType "4-suit" 4 4 25 [ 5, 5, 5, 5, 5 ]
            , GameType "5-suit" 5 5 28 [ 6, 6, 6, 5, 5 ]
            , GameType "1-suit Extra" 5 1 28 [ 6, 6, 6, 5, 5 ]
            ]

        indices =
            List.range 0 (List.length gameTypes - 1)
    in
    List.map2 Tuple.pair indices gameTypes |> Dict.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDeck cards ->
            ( { model
                | board = boardFromDeck model.gameType cards
              }
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
            , Random.generate NewDeck <| Random.List.shuffle <| deck newGameType
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

                        colIndex =
                            Board.tableauColumn model.board.tableau card
                    in
                    case colIndex of
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
                    case model.selection of
                        Spare c ->
                            if c == card then
                                NoSelection

                            else
                                Spare card

                        _ ->
                            Spare card
              }
            , Cmd.none
            )


updateMove : Board.MoveMsg -> Model -> ( Model, Cmd Msg )
updateMove msg model =
    case msg of
        Board.MoveTableauToTableau cards fromCol toCol ->
            case
                Board.validateTableauToTableau
                    model.board
                    cards
                    fromCol
                    toCol
            of
                ( True, Just False ) ->
                    ( { model
                        | board =
                            Board.moveTableauToTableau
                                model.board
                                cards
                                fromCol
                                toCol
                        , selection = NoSelection
                        , moves = model.moves + 1
                        , undoHistory = model.board :: model.undoHistory
                      }
                    , Cmd.none
                    )

                ( True, Just True ) ->
                    ( { model
                        | board =
                            Board.moveTableauToTableau
                                model.board
                                (List.reverse cards)
                                fromCol
                                toCol
                        , selection = NoSelection
                        , moves = model.moves + List.length cards
                        , undoHistory = model.board :: model.undoHistory
                      }
                    , Cmd.none
                    )

                ( _, _ ) ->
                    ( model, Cmd.none )

        Board.MoveSpareToTableau card toCol ->
            case Board.validateSpareToTableau model.board card toCol of
                True ->
                    ( { model
                        | board =
                            Board.moveSpareToTableau
                                model.board
                                card
                                toCol
                        , selection = NoSelection
                        , moves = model.moves + 1
                        , undoHistory = model.board :: model.undoHistory
                      }
                    , Cmd.none
                    )

                False ->
                    ( model, Cmd.none )

        Board.MoveSpareToFoundation card toFnd ->
            case Board.validateSpareToFoundation model.board card toFnd of
                True ->
                    ( { model
                        | board =
                            Board.moveSpareToFoundation model.board
                                card
                                toFnd
                        , selection = NoSelection
                        , moves = model.moves + 1
                        , undoHistory = model.board :: model.undoHistory
                      }
                    , Cmd.none
                    )

                False ->
                    ( model, Cmd.none )

        Board.MoveTableauToFoundation cards fromTab toFnd ->
            case
                Board.validateTableauToFoundation model.board
                    cards
                    fromTab
                    toFnd
            of
                True ->
                    ( { model
                        | board =
                            Board.moveTableauToFoundation
                                model.board
                                cards
                                fromTab
                                toFnd
                        , selection = NoSelection
                        , moves = model.moves + List.length cards
                        , undoHistory = model.board :: model.undoHistory
                      }
                    , Cmd.none
                    )

                False ->
                    ( model, Cmd.none )

        Board.MoveStockToTableau ->
            ( { model
                | board = Board.addCardsFromStock model.board
                , selection = NoSelection
                , moves = model.moves + 1
                , undoHistory = model.board :: model.undoHistory
              }
            , Cmd.none
            )


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
        [ padding <| floor (10 * scale), Background.color <| rgb255 157 120 85 ]
    <|
        row [ spacing <| floor (25 * scale), height fill ]
            [ viewMain model
            , viewSidebar model
            ]


viewMain : Model -> Element Msg
viewMain model =
    column
        [ spacing <| floor (25 * scale)
        , alignTop
        , width <| cardWidth 6
        ]
    <|
        case model.gameState of
            NewGame ->
                [ none ]

            Playing ->
                [ viewFoundations model, viewTableau model ]

            GameOver ->
                [ none ]


cardWidth : Float -> Length
cardWidth cards =
    px <| floor (68 * scale * cards)


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
    row [ spacing <| floor (10 * scale), centerX ] <| foundations ++ spacer


globalCardAtts : List (Attribute msg)
globalCardAtts =
    [ Border.rounded <| floor (4 * scale)
    , width (cardWidth 1)
    , height (cardHeight 1)
    ]


viewFoundation : Model -> Int -> List Card -> Element Msg
viewFoundation model foundation cards =
    let
        selectAtts =
            case model.selection of
                Spare spareCard ->
                    [ pointer
                    , Events.onClick <|
                        MoveMsg
                            (Board.MoveSpareToFoundation spareCard
                                foundation
                            )
                    ]

                Tableau tabCards tabCol ->
                    [ pointer
                    , Events.onClick <|
                        MoveMsg
                            (Board.MoveTableauToFoundation tabCards
                                tabCol
                                foundation
                            )
                    ]

                _ ->
                    []
    in
    case List.reverse cards of
        [] ->
            viewCardSpace selectAtts

        last :: _ ->
            viewCard model last selectAtts


viewCardSpace : List (Attribute Msg) -> Element Msg
viewCardSpace atts =
    el
        (globalCardAtts
            ++ atts
            ++ [ Background.color <| rgba 0 0 0 0.25 ]
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
        [ padding <| floor (3 * scale)
        , width fill
        , Font.size <| floor (20 * scale)
        , spacing <| floor (3 * scale)
        , Border.roundEach
            { topLeft = floor (4 * scale)
            , topRight = floor (4 * scale)
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
            el [ centerX, Font.size <| floor (22 * scale), Font.bold ] <|
                text "FlipFlop"

        sidebarBurger =
            Input.button [ centerX, Font.size <| floor (25 * scale) ]
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
    [ spacing <| floor (25 * scale)
    , alignTop
    , width <| cardWidth 2.5
    , height fill
    , padding <| floor (10 * scale)
    , Background.color <| rgba 0 0 0 0.25
    , Font.size <| floor (15 * scale)
    , Font.color <| rgb 1 1 1
    ]


viewSelectGame : Element Msg
viewSelectGame =
    let
        newGameLink gameType =
            Input.button [ centerX ]
                { onPress = Just (StartGame <| getGameType gameType)
                , label = text <| .name <| getGameType gameType
                }
    in
    column [ spacing <| floor (20 * scale), centerX ] <|
        List.map newGameLink (Dict.keys validGameTypes)


viewInfo : Model -> Element Msg
viewInfo model =
    let
        numFoundationCards =
            model.board.foundations |> List.concat |> List.length |> toFloat

        numTargetCards =
            model.gameType.numFoundations * 13 |> toFloat
    in
    column
        [ Font.size <| floor (15 * scale)
        , spacing <| floor (10 * scale)
        , Font.color <| rgb 1 1 1
        , centerX
        , height <| cardHeight 1.25
        ]
        [ el [ centerX, Font.size <| floor (18 * scale), Font.bold ] <|
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
    row [ spacing <| floor (10 * scale), centerX ] <|
        List.map
            (viewTableauColumn model)
            (Dict.keys model.board.tableau)


viewTableauColumn : Model -> Int -> Element Msg
viewTableauColumn model colIndex =
    Board.getTableauColumn model.board.tableau colIndex
        |> viewColumn model colIndex


viewColumn : Model -> Int -> List Card -> Element Msg
viewColumn model colIndex cards =
    let
        selAtts =
            case model.selection of
                Tableau tabCards tabCol ->
                    [ pointer
                    , if colIndex == tabCol then
                        Events.onClick (SelectMsg ClearSelection)

                      else
                        Events.onClick <|
                            MoveMsg
                                (Board.MoveTableauToTableau
                                    tabCards
                                    tabCol
                                    colIndex
                                )
                    ]

                Spare spareCard ->
                    [ pointer
                    , Events.onClick <|
                        MoveMsg (Board.MoveSpareToTableau spareCard colIndex)
                    ]

                _ ->
                    []

        warningAtts =
            if List.length cards >= 20 then
                [ Border.color <| rgb255 250 100 42
                , Border.widthEach { bottom = 5, top = 0, right = 0, left = 0 }
                , Border.solid
                ]

            else
                []
    in
    column
        ([ alignTop, spacing -(floor <| 81 * scale) ]
            ++ selAtts
            ++ warningAtts
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
    row [ spacing <| floor (10 * scale) ]
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
                el
                    [ inFront <| viewStockRow rest
                    , moveRight 15
                    ]
                <|
                    first


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
            [ Border.rounded <| floor (4 * scale / innerScale)
            , width <| px <| floor (68 * scale / innerScale)
            , height <| px <| floor (105 * scale / innerScale)
            , Background.color <| rgb255 44 49 64
            , centerX
            , centerY
            ]
        <|
            none


cardHeight : Float -> Length
cardHeight cards =
    px <| floor (105 * scale * cards)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


colWarningColumn : Color
colWarningColumn =
    rgb255 250 100 42
