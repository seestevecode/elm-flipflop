module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
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
            { name = "4-suit"
            , numFoundations = 4
            , numSuits = 1
            , numTableauCards = 25
            , tableauColSizes = [ 5, 5, 5, 5, 5 ]
            }
    in
    ( { gameType = gameType
      , board = boardFromDeck gameType <| deck gameType
      , selection = NothingSelected
      }
    , Random.generate NewDeck <|
        Random.List.shuffle (deck gameType)
    )


type alias Model =
    { gameType : GameType
    , board : Board
    , selection : Selection
    }


type alias GameType =
    { name : String
    , numFoundations : Int
    , numSuits : Int
    , numTableauCards : Int
    , tableauColSizes : List Int
    }


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
    | SelectSpare Card
    | SelectTableau Card


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
    [ Spades, Hearts, Clubs, Diamonds, Stars ]


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
              }
            , Cmd.none
            )

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
        |> ListX.groupWhile cardsLink
        |> List.map (\( x, xs ) -> x :: xs)


cardsLink : Card -> Card -> Bool
cardsLink x y =
    let
        consecutiveRanks =
            ListX.zip orderedRanks (List.drop 1 orderedRanks)
    in
    (List.member ( x.rank, y.rank ) consecutiveRanks
        || List.member ( y.rank, x.rank ) consecutiveRanks
    )
        && (x.suit == y.suit)


scale : Float
scale =
    1


view : Model -> Html Msg
view model =
    layout
        [ padding <| floor (10 * scale)
        , Background.color <| rgb255 157 120 85
        , inFront <|
            row
                [ width fill, alignBottom, padding <| floor (10 * scale) ]
                [ el [] <| viewSpare model, el [] <| viewStock model ]
        ]
    <|
        column [ spacing <| floor (25 * scale) ]
            [ viewFoundations model, viewTableau model ]


viewFoundations : Model -> Element Msg
viewFoundations model =
    row [ spacing <| floor (10 * scale) ] <|
        List.indexedMap
            (\foundationIndex foundationCards ->
                case List.reverse foundationCards of
                    [] ->
                        viewCardSpace

                    last :: _ ->
                        viewCard model last []
            )
            model.board.foundations


viewTableau : Model -> Element Msg
viewTableau model =
    row [ spacing <| floor (10 * scale) ] <|
        List.map
            (viewTableauColumn model)
            (Dict.keys model.board.tableau)


viewTableauColumn : Model -> Int -> Element Msg
viewTableauColumn model colIndex =
    getTableauColumn model.board.tableau colIndex |> viewColumn model


viewColumn : Model -> List Card -> Element Msg
viewColumn model cards =
    column [ alignTop, spacing -(floor <| 81 * scale) ] <|
        case cards of
            [] ->
                [ el globalCardAtts none ]

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
                    viewCardSpace

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


viewCardSpace : Element Msg
viewCardSpace =
    el (globalCardAtts ++ [ Background.color <| rgba 0 0 0 0.25 ]) <| none


globalCardAtts : List (Attribute Msg)
globalCardAtts =
    [ Border.rounded <| floor (4 * scale)
    , width <| px <| floor (68 * scale)
    , height <| px <| floor (105 * scale)
    ]


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
            ( "♣", rgb255 54 55 36 )

        Diamonds ->
            ( "♦", rgb255 242 168 31 )

        Spades ->
            ( "♠", rgb255 114 147 181 )

        Stars ->
            ( "★", rgb255 109 167 128 )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
