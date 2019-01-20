module Card exposing
    ( Card
    , Orientation(..)
    , Rank(..)
    , Suit(..)
    , bodyFontSize
    , bodyPadding
    , cardBackColour
    , cardCornerRound
    , cardHeight
    , cardSpaceColour
    , cardWidth
    , cardsLinkFoundationBuild
    , cardsLinkTableauBuild
    , cardsLinkTableauMove
    , clubsColour
    , consecutiveRanks
    , diamondsColour
    , globalCardAtts
    , groupCardsFoundationMove
    , groupCardsTableauMove
    , groupCardsToMove
    , heartsColour
    , orderedRanks
    , orderedSuits
    , selectionValidFoundationMove
    , selectionValidTableauMove
    , spadesColour
    , starsColour
    , suitOutput
    , tailFromCard
    , turnUp
    , viewCardFacedown
    , viewCardFaceupBody
    , viewCardSpace
    , viewRank
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
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


tailFromCard : Card -> List Card -> List Card
tailFromCard targetCard cards =
    case cards of
        [] ->
            []

        x :: xs ->
            if x == targetCard then
                cards

            else
                tailFromCard targetCard xs



-- Selection valid checks


selectionValidTableauMove : List Card -> Bool
selectionValidTableauMove cards =
    (List.length <| groupCardsTableauMove cards) == 1


selectionValidFoundationMove : List Card -> Bool
selectionValidFoundationMove cards =
    (List.length <| groupCardsFoundationMove cards) == 1



-- Group cards


groupCardsToMove : (Card -> Card -> Bool) -> List Card -> List (List Card)
groupCardsToMove cardsLink cards =
    cards
        |> ListX.groupWhile cardsLink
        |> List.map (\( x, xs ) -> x :: xs)


groupCardsTableauMove : List Card -> List (List Card)
groupCardsTableauMove cards =
    groupCardsToMove cardsLinkTableauMove cards


groupCardsFoundationMove : List Card -> List (List Card)
groupCardsFoundationMove cards =
    groupCardsToMove cardsLinkFoundationBuild cards



-- Cards link checks


cardsLinkTableauBuild : Card -> Card -> Bool
cardsLinkTableauBuild x y =
    List.member ( x.rank, y.rank ) consecutiveRanks
        || List.member ( y.rank, x.rank ) consecutiveRanks


cardsLinkTableauMove : Card -> Card -> Bool
cardsLinkTableauMove x y =
    cardsLinkTableauBuild x y && (x.suit == y.suit)


cardsLinkFoundationBuild : Card -> Card -> Bool
cardsLinkFoundationBuild x y =
    List.member ( x.rank, y.rank ) consecutiveRanks
        && (x.suit == y.suit)



-- View card


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


suitOutput : Suit -> ( String, Color )
suitOutput suit =
    case suit of
        Hearts ->
            ( "♥", heartsColour )

        Clubs ->
            ( "♣", clubsColour )

        Diamonds ->
            ( "♦", diamondsColour )

        Spades ->
            ( "♠", spadesColour )

        Stars ->
            ( "★", starsColour )


viewCardFaceupBody : Card -> Element msg
viewCardFaceupBody card =
    el
        [ Font.size bodyFontSize
        , centerX
        , Font.color <| Tuple.second <| suitOutput card.suit
        , paddingEach { bottom = bodyPadding, left = 0, right = 0, top = 0 }
        ]
    <|
        text <|
            Tuple.first <|
                suitOutput card.suit


viewCardFacedown : Element msg
viewCardFacedown =
    let
        innerScale =
            1.05
    in
    el ((Background.color <| rgb 1 1 1) :: globalCardAtts) <|
        el
            [ Border.rounded <| floor <| toFloat cardCornerRound / innerScale
            , width <| px <| floor <| toFloat cardWidth / innerScale
            , height <| px <| floor <| toFloat cardHeight / innerScale
            , Background.color cardBackColour
            , centerX
            , centerY
            ]
        <|
            none


viewCardSpace : List (Attribute msg) -> Element msg
viewCardSpace atts =
    el ([ Background.color <| cardSpaceColour ] ++ globalCardAtts ++ atts) <|
        none


globalCardAtts : List (Attribute msg)
globalCardAtts =
    [ Border.rounded cardCornerRound
    , width <| px cardWidth
    , height <| px cardHeight
    ]



-- Constants


bodyFontSize : Int
bodyFontSize =
    75


bodyPadding : Int
bodyPadding =
    10


heartsColour : Color
heartsColour =
    rgb255 218 87 53


clubsColour : Color
clubsColour =
    rgb255 114 147 181


diamondsColour : Color
diamondsColour =
    rgb255 242 168 31


spadesColour : Color
spadesColour =
    rgb255 54 55 36


starsColour : Color
starsColour =
    rgb255 109 167 128


cardBackColour : Color
cardBackColour =
    rgb255 44 49 64


cardCornerRound : Int
cardCornerRound =
    4


cardHeight : Int
cardHeight =
    105


cardWidth : Int
cardWidth =
    68


cardSpaceColour : Color
cardSpaceColour =
    rgba 0 0 0 0.25
