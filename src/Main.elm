module Main exposing (main)

import Browser
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


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        gameType =
            { name = "1-suit"
            , numFoundations = 4
            , numSuits = 1
            , numTableauCards = 25
            , tableauColSizes = [ 5, 5, 5, 5, 5 ]
            }
    in
    ( { gameType = gameType
      , board = boardFromDeck gameType <| deck gameType
      , selection = NothingSelected
      , moves = 0
      , undoHistory = []
      , undoUsed = False
      }
    , Random.generate NewDeck <|
        Random.List.shuffle (deck gameType)
    )


type alias Model =
    { gameType : GameType
    , board : Board
    , selection : Selection
    , moves : Int
    , undoHistory : List Board
    , undoUsed : Bool
    }


type alias GameType =
    { name : String
    , numFoundations : Int
    , numSuits : Int
    , numTableauCards : Int
    , tableauColSizes : List Int
    }


validGameTypes : List GameType
validGameTypes =
    [ GameType "1-suit" 4 1 25 [ 5, 5, 5, 5, 5 ]
    , GameType "2-suit" 4 2 25 [ 5, 5, 5, 5, 5 ]
    , GameType "3-suit" 4 3 25 [ 5, 5, 5, 5, 5 ]
    , GameType "4-suit" 4 4 25 [ 5, 5, 5, 5, 5 ]
    , GameType "5-suit" 5 5 28 [ 6, 6, 6, 5, 5 ]
    , GameType "1-suit Extra" 5 1 28 [ 6, 6, 6, 5, 5 ]
    ]


type alias Board =
    { foundations : List (List Card)
    , tableau : Tableau
    , spare : ( Maybe Card, Maybe Card )
    , stock : List (List Card)
    }


type alias Card =
    { rank : Rank, suit : Suit, faceUp : Bool, id : Int }


type Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type Suit
    = Spades
    | Hearts
    | Clubs
    | Diamonds
    | Stars


type alias Tableau =
    Dict Int (List Card)


type Selection
    = NothingSelected
    | SingleSpare Card
    | SingleTableau Card Int
    | ManyTableau (List Card) Int


type Msg
    = NewDeck (List Card)
    | AddCardsFromStock
    | ClearSelection
    | SelectSpare Card
    | SelectTableau Card
    | MoveTableauToTableau (List Card) Int Int
    | MoveSpareToTableau Card Int
    | MoveSpareToFoundation Card Int
    | MoveTableauToFoundation Card Int Int
    | Undo
    | Restart


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
                    |> List.map turnUp
        in
        ( List.head spareCards, ListX.last spareCards )
    , stock =
        cards
            |> List.drop (gameType.numTableauCards + 2)
            |> ListX.groupsOf 5
    }


buildTableau : GameType -> List Card -> Tableau
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
        |> turnUpEndCards


deck : GameType -> List Card
deck gameType =
    let
        ranks =
            List.repeat gameType.numFoundations orderedRanks |> List.concat

        suits =
            ListX.cycle
                gameType.numFoundations
                (List.take gameType.numSuits orderedSuits)
                |> List.repeat 13
                |> List.concat
                |> ListX.gatherEquals
                |> List.concatMap (\( c, cs ) -> c :: cs)

        allFaceDown =
            List.repeat (gameType.numFoundations * 13) False

        ids =
            List.range 1 (gameType.numFoundations * 13)
    in
    List.map4 Card ranks suits allFaceDown ids


orderedRanks : List Rank
orderedRanks =
    [ Ace
    , Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten
    , Jack
    , Queen
    , King
    ]


orderedSuits : List Suit
orderedSuits =
    [ Spades, Hearts, Diamonds, Clubs, Stars ]


turnUpEndCards : Tableau -> Tableau
turnUpEndCards tableau =
    tableau
        |> Dict.map
            (\_ cards ->
                case List.reverse cards of
                    [] ->
                        []

                    last :: restReversed ->
                        turnUp last :: restReversed |> List.reverse
            )


turnUp : Card -> Card
turnUp card =
    { card | faceUp = True }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDeck cards ->
            ( { model
                | board = boardFromDeck model.gameType cards
              }
            , Cmd.none
            )

        AddCardsFromStock ->
            ( { model
                | board = addCardsFromStock model.board
                , selection = NothingSelected
                , moves = model.moves + 1
                , undoHistory = model.board :: model.undoHistory
              }
            , Cmd.none
            )

        ClearSelection ->
            ( { model | selection = NothingSelected }, Cmd.none )

        SelectSpare card ->
            ( { model
                | selection =
                    case model.selection of
                        SingleSpare c ->
                            if c == card then
                                NothingSelected

                            else
                                SingleSpare card

                        _ ->
                            SingleSpare card
              }
            , Cmd.none
            )

        SelectTableau card ->
            ( { model
                | selection =
                    let
                        cards =
                            selectFromCardInTableau card model.board.tableau

                        colIndex =
                            tableauColumn model.board.tableau card
                    in
                    case colIndex of
                        Just col ->
                            if selectionValid cards then
                                case cards of
                                    [ c ] ->
                                        SingleTableau c col

                                    cs ->
                                        ManyTableau cs col

                            else
                                model.selection

                        Nothing ->
                            model.selection
              }
            , Cmd.none
            )

        MoveTableauToTableau cards fromCol toCol ->
            case validateTableauToTableau model.board cards fromCol toCol of
                True ->
                    ( { model
                        | board =
                            moveTableauToTableau model.board cards fromCol toCol
                        , selection = NothingSelected
                        , moves = model.moves + 1
                        , undoHistory = model.board :: model.undoHistory
                      }
                    , Cmd.none
                    )

                False ->
                    ( model, Cmd.none )

        MoveSpareToTableau card toCol ->
            case validateSpareToTableau model.board card toCol of
                True ->
                    ( { model
                        | board = moveSpareToTableau model.board card toCol
                        , selection = NothingSelected
                        , moves = model.moves + 1
                        , undoHistory = model.board :: model.undoHistory
                      }
                    , Cmd.none
                    )

                False ->
                    ( model, Cmd.none )

        MoveSpareToFoundation card toFnd ->
            case validateSpareToFoundation model.board card toFnd of
                True ->
                    ( { model
                        | board = moveSpareToFoundation model.board card toFnd
                        , selection = NothingSelected
                        , moves = model.moves + 1
                        , undoHistory = model.board :: model.undoHistory
                      }
                    , Cmd.none
                    )

                False ->
                    ( model, Cmd.none )

        MoveTableauToFoundation card fromTab toFnd ->
            case validateTableauToFoundation model.board card fromTab toFnd of
                True ->
                    ( { model
                        | board = moveTableauToFoundation model.board card fromTab toFnd
                        , selection = NothingSelected
                        , moves = model.moves + 1
                        , undoHistory = model.board :: model.undoHistory
                      }
                    , Cmd.none
                    )

                False ->
                    ( model, Cmd.none )

        Undo ->
            case model.undoHistory of
                [] ->
                    ( model, Cmd.none )

                last :: rest ->
                    ( { model
                        | board = last
                        , selection = NothingSelected
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
                        , selection = NothingSelected
                        , undoHistory = []
                        , undoUsed = True
                      }
                    , Cmd.none
                    )


validateTableauToTableau : Board -> List Card -> Int -> Int -> Bool
validateTableauToTableau board cards fromCol toCol =
    let
        source =
            List.head cards

        destination =
            ListX.last <| getTableauColumn board.tableau toCol
    in
    case ( source, destination ) of
        ( Just s, Just d ) ->
            cardsLinkTableauBuild s d

        ( Just s, Nothing ) ->
            True

        ( Nothing, _ ) ->
            False


moveTableauToTableau : Board -> List Card -> Int -> Int -> Board
moveTableauToTableau board cards fromCol toCol =
    { board
        | tableau =
            board.tableau
                |> Dict.map
                    (\k cs ->
                        if k == fromCol then
                            cs
                                |> List.reverse
                                |> List.drop (List.length cards)
                                |> List.reverse

                        else if k == toCol then
                            cs ++ cards

                        else
                            cs
                    )
                |> turnUpEndCards
    }


validateSpareToTableau : Board -> Card -> Int -> Bool
validateSpareToTableau board card toCol =
    let
        destination =
            ListX.last <| getTableauColumn board.tableau toCol
    in
    case destination of
        Just d ->
            cardsLinkTableauBuild card d

        Nothing ->
            True


moveSpareToTableau : Board -> Card -> Int -> Board
moveSpareToTableau board card toCol =
    { board
        | tableau =
            board.tableau
                |> Dict.map
                    (\k cs ->
                        if k == toCol then
                            cs ++ [ card ]

                        else
                            cs
                    )
        , spare =
            let
                removeSelectedSpare =
                    \c ->
                        if c == Just card then
                            Nothing

                        else
                            c
            in
            board.spare
                |> Tuple.mapBoth removeSelectedSpare removeSelectedSpare
    }


validateSpareToFoundation : Board -> Card -> Int -> Bool
validateSpareToFoundation board card toFnd =
    let
        destination =
            board.foundations
                |> ListX.getAt toFnd
                |> Maybe.withDefault []
                |> ListX.last
    in
    case destination of
        Just c ->
            cardsLinkFoundationBuild c card

        Nothing ->
            card.rank == Ace


moveSpareToFoundation : Board -> Card -> Int -> Board
moveSpareToFoundation board card toFnd =
    { board
        | foundations =
            board.foundations
                |> List.indexedMap
                    (\f cs ->
                        if f == toFnd then
                            cs ++ [ card ]

                        else
                            cs
                    )
        , spare =
            let
                removeSelectedSpare =
                    \c ->
                        if c == Just card then
                            Nothing

                        else
                            c
            in
            board.spare |> Tuple.mapBoth removeSelectedSpare removeSelectedSpare
    }


validateTableauToFoundation : Board -> Card -> Int -> Int -> Bool
validateTableauToFoundation board card fromTab toFnd =
    let
        destination =
            board.foundations
                |> ListX.getAt toFnd
                |> Maybe.withDefault []
                |> ListX.last
    in
    case destination of
        Just c ->
            cardsLinkFoundationBuild c card

        Nothing ->
            card.rank == Ace


moveTableauToFoundation : Board -> Card -> Int -> Int -> Board
moveTableauToFoundation board card fromTab toFnd =
    { board
        | tableau =
            board.tableau
                |> Dict.map
                    (\k cs ->
                        if k == fromTab then
                            cs |> List.reverse |> List.drop 1 |> List.reverse

                        else
                            cs
                    )
                |> turnUpEndCards
        , foundations =
            board.foundations
                |> List.indexedMap
                    (\foundationIndex cs ->
                        if foundationIndex == toFnd then
                            cs ++ [ card ]

                        else
                            cs
                    )
    }


addCardsFromStock : Board -> Board
addCardsFromStock board =
    { board
        | tableau =
            let
                tableauList : List ( Int, List Card )
                tableauList =
                    Dict.toList board.tableau

                stockList : List Card
                stockList =
                    List.take 1 board.stock
                        |> List.concat
            in
            List.map2
                (\( k, tabCards ) stockCard ->
                    ( k, tabCards ++ List.singleton stockCard )
                )
                tableauList
                stockList
                |> Dict.fromList
                |> turnUpEndCards
        , stock = List.drop 1 board.stock
    }


tableauColumn : Tableau -> Card -> Maybe Int
tableauColumn tableau card =
    tableau
        |> Dict.filter (\_ cs -> List.member card cs)
        |> Dict.keys
        |> List.head


selectFromCardInTableau : Card -> Tableau -> List Card
selectFromCardInTableau card tableau =
    Dict.foldl
        (\_ column tail ->
            case tail of
                [] ->
                    selectFromCardInColumn card column

                _ ->
                    tail
        )
        []
        tableau


selectFromCardInColumn : Card -> List Card -> List Card
selectFromCardInColumn card column =
    case column of
        [] ->
            []

        [ c ] ->
            if c == card then
                [ c ]

            else
                []

        cs ->
            ListX.dropWhile (\c -> c /= card) cs


selectionValid : List Card -> Bool
selectionValid cards =
    (List.length <| groupCardsByLink cards) == 1


groupCardsByLink : List Card -> List (List Card)
groupCardsByLink cards =
    cards
        |> ListX.groupWhile cardsLinkTableauMove
        |> List.map (\( x, xs ) -> x :: xs)


cardsLinkTableauMove : Card -> Card -> Bool
cardsLinkTableauMove x y =
    (List.member ( x.rank, y.rank ) consecutiveRanks
        || List.member ( y.rank, x.rank ) consecutiveRanks
    )
        && (x.suit == y.suit)


cardsLinkTableauBuild : Card -> Card -> Bool
cardsLinkTableauBuild x y =
    List.member ( x.rank, y.rank ) consecutiveRanks
        || List.member ( y.rank, x.rank ) consecutiveRanks


cardsLinkFoundationBuild : Card -> Card -> Bool
cardsLinkFoundationBuild x y =
    List.member ( x.rank, y.rank ) consecutiveRanks
        && (x.suit == y.suit)


consecutiveRanks : List ( Rank, Rank )
consecutiveRanks =
    ListX.zip orderedRanks (List.drop 1 orderedRanks)


scale : Float
scale =
    1


view : Model -> Html Msg
view model =
    layout
        [ padding <| floor (10 * scale)
        , Background.color <| rgb255 157 120 85
        ]
    <|
        row [ spacing <| floor (25 * scale), height fill ]
            [ column [ spacing <| floor (25 * scale), alignTop ]
                [ viewFoundations model
                , viewTableau model
                ]
            , column
                [ spacing <| floor (25 * scale)
                , alignTop
                , width fill
                , height fill
                , padding <| floor (10 * scale)
                , Background.color <| rgba 0 0 0 0.25
                , Font.size <| floor (15 * scale)
                , Font.color <| rgb 1 1 1
                , inFront <|
                    if List.length model.undoHistory >= 1 then
                        row
                            [ alignBottom
                            , width fill
                            , padding <| floor (10 * scale)
                            ]
                            [ viewUndoButton, viewRestartButton ]

                    else
                        none
                ]
                [ viewInfo model
                , el [ centerX ] <| text "☰"
                , column [ spacing <| floor (20 * scale) ]
                    [ viewSpare model, viewStock model ]
                , el [] <|
                    paragraph [ alignTop ]
                        [ text "Some stuff goes in here." ]
                ]
            ]


viewFoundations : Model -> Element Msg
viewFoundations model =
    row [ spacing <| floor (10 * scale) ] <|
        List.indexedMap
            (\foundationIndex foundationCards ->
                let
                    selectAtts =
                        case model.selection of
                            SingleSpare spareCard ->
                                [ pointer
                                , Events.onClick
                                    (MoveSpareToFoundation
                                        spareCard
                                        foundationIndex
                                    )
                                ]

                            SingleTableau tabCard tabCol ->
                                [ pointer
                                , Events.onClick
                                    (MoveTableauToFoundation
                                        tabCard
                                        tabCol
                                        foundationIndex
                                    )
                                ]

                            _ ->
                                []
                in
                case List.reverse foundationCards of
                    [] ->
                        viewCardSpace selectAtts

                    last :: _ ->
                        viewCard model last selectAtts
            )
            model.board.foundations


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
        ]
        [ el [ centerX, Font.size <| floor (22 * scale), Font.bold ] <|
            text "FlipFlop"
        , el [ centerX, Font.size <| floor (18 * scale), Font.bold ] <|
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
    row [ spacing <| floor (10 * scale) ] <|
        List.map
            (viewTableauColumn model)
            (Dict.keys model.board.tableau)


viewTableauColumn : Model -> Int -> Element Msg
viewTableauColumn model colIndex =
    getTableauColumn model.board.tableau colIndex |> viewColumn model colIndex


viewColumn : Model -> Int -> List Card -> Element Msg
viewColumn model colIndex cards =
    let
        selAtts =
            case model.selection of
                SingleTableau tabCard tabCol ->
                    [ pointer
                    , if colIndex == tabCol then
                        Events.onClick ClearSelection

                      else
                        Events.onClick
                            (MoveTableauToTableau [ tabCard ] tabCol colIndex)
                    ]

                ManyTableau tabCards tabCol ->
                    [ pointer
                    , if colIndex == tabCol then
                        Events.onClick ClearSelection

                      else
                        Events.onClick
                            (MoveTableauToTableau tabCards tabCol colIndex)
                    ]

                SingleSpare spareCard ->
                    [ pointer
                    , Events.onClick (MoveSpareToTableau spareCard colIndex)
                    ]

                _ ->
                    []
    in
    column ([ alignTop, spacing -(floor <| 81 * scale) ] ++ selAtts) <|
        case cards of
            [] ->
                [ viewCardSpace [] ]

            cs ->
                let
                    viewTabCard c =
                        viewCard model
                            c
                            [ pointer, Events.onClick (SelectTableau c) ]
                in
                List.map viewTabCard cs


getTableauColumn : Tableau -> Int -> List Card
getTableauColumn tableau colIndex =
    tableau
        |> Dict.filter (\k _ -> k == colIndex)
        |> Dict.values
        |> List.concat


viewSpare : Model -> Element Msg
viewSpare model =
    let
        viewSingleSpare spare =
            case spare of
                Nothing ->
                    el globalCardAtts none

                Just s ->
                    viewCard model s [ Events.onClick <| SelectSpare s, pointer ]

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
    el [ Events.onClick AddCardsFromStock, pointer ] <|
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


viewCard : Model -> Card -> List (Attribute Msg) -> Element Msg
viewCard model card attr =
    case card.faceUp of
        True ->
            viewCardFaceup model card attr

        False ->
            viewCardFacedown


viewCardFaceup : Model -> Card -> List (Attribute Msg) -> Element Msg
viewCardFaceup model card attr =
    column
        (globalCardAtts
            ++ attr
            ++ [ Background.color <| rgb 1 1 1 ]
        )
        [ viewCardFaceupHead model card, viewCardFaceupBody card ]


viewCardFaceupHead : Model -> Card -> Element Msg
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
        , Background.color <| Tuple.second <| suitOutput card.suit
        , Font.color <| rgb 1 1 1
        ]
        [ viewRank card.rank
        , el [] <|
            text <|
                Tuple.first <|
                    suitOutput card.suit
        , if cardSelected model.selection card then
            el [ alignRight, Font.color <| rgb 1 1 1 ] <| text "●"

          else
            none
        ]


cardSelected : Selection -> Card -> Bool
cardSelected selection card =
    case selection of
        SingleSpare spareCard ->
            card == spareCard

        SingleTableau tabCard _ ->
            card == tabCard

        ManyTableau tabCards _ ->
            List.member card tabCards

        NothingSelected ->
            False


viewCardFaceupBody : Card -> Element Msg
viewCardFaceupBody card =
    el
        [ Font.size <| floor (75 * scale)
        , centerX
        , Font.color <| Tuple.second <| suitOutput card.suit
        , paddingEach
            { bottom = floor (10 * scale)
            , left = 0
            , right = 0
            , top = 0
            }
        ]
    <|
        text <|
            Tuple.first <|
                suitOutput card.suit


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


viewCardSpace : List (Attribute Msg) -> Element Msg
viewCardSpace atts =
    el
        (globalCardAtts
            ++ atts
            ++ [ Background.color <| rgba 0 0 0 0.25 ]
        )
    <|
        none


globalCardAtts : List (Attribute Msg)
globalCardAtts =
    [ Border.rounded <| floor (4 * scale)
    , width (cardWidth 1)
    , height (cardHeight 1)
    ]


cardWidth : Int -> Length
cardWidth cards =
    px <| floor (68 * scale * toFloat cards)


cardHeight : Int -> Length
cardHeight cards =
    px <| floor (105 * scale * toFloat cards)


viewRank : Rank -> Element Msg
viewRank rank =
    el [] <|
        text <|
            case rank of
                Ace ->
                    "A"

                Two ->
                    "2"

                Three ->
                    "3"

                Four ->
                    "4"

                Five ->
                    "5"

                Six ->
                    "6"

                Seven ->
                    "7"

                Eight ->
                    "8"

                Nine ->
                    "9"

                Ten ->
                    "10"

                Jack ->
                    "J"

                Queen ->
                    "Q"

                King ->
                    "K"


suitOutput : Suit -> ( String, Color )
suitOutput suit =
    case suit of
        Hearts ->
            ( "♥", rgb255 218 87 53 )

        Clubs ->
            ( "♣", rgb255 114 147 181 )

        Diamonds ->
            ( "♦", rgb255 242 168 31 )

        Spades ->
            ( "♠", rgb255 54 55 36 )

        Stars ->
            ( "★", rgb255 109 167 128 )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
