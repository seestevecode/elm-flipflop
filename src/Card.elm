module Card exposing
    ( Card
    , Orientation(..)
    , Rank(..)
    , Suit(..)
    , cardsLinkFoundationBuild
    , cardsLinkTableauBuild
    , cardsLinkTableauMove
    , consecutiveRanks
    , groupCardsFoundationMove
    , groupCardsTableauMove
    , orderedRanks
    , orderedSuits
    , selectFromCardInColumn
    , selectionValidFoundationMove
    , selectionValidTableauMove
    , suitOutput
    , turnUp
    , viewCardFaceupBody
    , viewRank
    )

import Element exposing (..)
import Element.Font as Font
import List.Extra as ListX


type alias Card =
    { rank : Rank, suit : Suit, orientation : Orientation, id : Int }


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


type Orientation
    = FaceUp
    | FaceDown


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


turnUp : Card -> Card
turnUp card =
    { card | orientation = FaceUp }


consecutiveRanks : List ( Rank, Rank )
consecutiveRanks =
    ListX.zip orderedRanks (List.drop 1 orderedRanks)


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


cardsLinkTableauBuild : Card -> Card -> Bool
cardsLinkTableauBuild x y =
    List.member ( x.rank, y.rank ) consecutiveRanks
        || List.member ( y.rank, x.rank ) consecutiveRanks


selectionValidTableauMove : List Card -> Bool
selectionValidTableauMove cards =
    (List.length <| groupCardsTableauMove cards) == 1


groupCardsTableauMove : List Card -> List (List Card)
groupCardsTableauMove cards =
    cards
        |> ListX.groupWhile cardsLinkTableauMove
        |> List.map (\( x, xs ) -> x :: xs)


cardsLinkTableauMove : Card -> Card -> Bool
cardsLinkTableauMove x y =
    (List.member ( x.rank, y.rank ) consecutiveRanks
        || List.member ( y.rank, x.rank ) consecutiveRanks
    )
        && (x.suit == y.suit)


selectionValidFoundationMove : List Card -> Bool
selectionValidFoundationMove cards =
    (List.length <| groupCardsFoundationMove cards) == 1


groupCardsFoundationMove : List Card -> List (List Card)
groupCardsFoundationMove cards =
    cards
        |> ListX.groupWhile cardsLinkFoundationBuild
        |> List.map (\( x, xs ) -> x :: xs)


cardsLinkFoundationBuild : Card -> Card -> Bool
cardsLinkFoundationBuild x y =
    List.member ( x.rank, y.rank ) consecutiveRanks
        && (x.suit == y.suit)


viewRank : Rank -> Element msg
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


viewCardFaceupBody : Card -> Element msg
viewCardFaceupBody card =
    el
        [ Font.size <| floor 75
        , centerX
        , Font.color <| Tuple.second <| suitOutput card.suit
        , paddingEach
            { bottom = floor 10
            , left = 0
            , right = 0
            , top = 0
            }
        ]
    <|
        text <|
            Tuple.first <|
                suitOutput card.suit
