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
            , numSuits = 4
            , numTableauCards = 25
            , tableauColSizes = [ 5, 5, 5, 5, 5 ]
            }
    in
    ( { gameType = gameType
      , board = boardFromDeck gameType <| deck gameType
      }
    , Random.generate NewDeck <|
        Random.List.shuffle (deck gameType)
    )


type alias Model =
    { gameType : GameType
    , board : Board
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


type Msg
    = NewDeck (List Card)
    | AddCardsFromStock


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
    |> Dict.map (\_ cards ->
        case List.reverse cards of
            [] -> []
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
        column [ spacing <| floor (25 * scale) ]
            [ viewFoundations model
            , viewTableau model
            , row [ width fill ]
                [ el [ alignLeft ] <| viewSpare model
                , el [ alignRight ] <| viewStock model
                ]
            ]


viewFoundations : Model -> Element Msg
viewFoundations model =
    row [ spacing <| floor (10 * scale) ] <|
        List.indexedMap
            (\foundationIndex foundationCards ->
                case List.reverse foundationCards of
                    [] ->
                        viewCardSpace

                    last :: _ ->
                        viewCard last
            )
            model.board.foundations


viewTableau : Model -> Element Msg
viewTableau model =
    row
        [ spacing <| floor (10 * scale)
        , height <| px 600
        ]
    <|
        List.map
            (viewTableauColumn model.board.tableau)
            (Dict.keys model.board.tableau)


viewTableauColumn : Tableau -> Int -> Element Msg
viewTableauColumn tableau colIndex =
    getTableauColumn tableau colIndex |> viewColumn


viewColumn : List Card -> Element Msg
viewColumn cards =
    let
        col =
            case cards of
                [] ->
                    el globalCardAtts none

                first :: rest ->
                    el
                        [ inFront <| viewColumn rest
                        , moveDown <| 24 * scale
                        ]
                    <|
                        viewCard first
    in
    column [ alignTop ] [ col ]


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
                    viewCard s
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
    let
        rowContent =
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
    in
    row [] [ rowContent ]


viewCard : Card -> Element Msg
viewCard card =
    case card.faceUp of
        True ->
            viewCardFaceup card

        False ->
            viewCardFacedown


viewCardFaceup : Card -> Element Msg
viewCardFaceup card =
    column
        (globalCardAtts ++ [ Background.color <| rgb 1 1 1 ])
        [ viewCardFaceupHead card, viewCardFaceupBody card ]


viewCardFaceupHead : Card -> Element Msg
viewCardFaceupHead card =
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
        ]


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
