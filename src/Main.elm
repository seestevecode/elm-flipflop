module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)


main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    [ Card Ace Diamonds
    , Card Ten Spades
    , Card Jack Clubs
    , Card Queen Hearts
    , Card King Stars
    ]


type alias Model =
    List Card


type alias Card =
    { rank : Rank, suit : Suit }


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


update : Model -> msg -> Model
update model msg =
    model


scale : Float
scale =
    1


round : Int
round =
    floor <| 4 * scale


view : Model -> Html msg
view model =
    layout
        [ padding <| floor (10 * scale)
        , Background.color <| rgb255 157 120 85
        ]
    <|
        row [ spacing <| floor (10 * scale) ] <|
            List.map viewCard model


viewCard : Card -> Element msg
viewCard card =
    column
        [ Border.rounded round
        , Background.color <| rgb 1 1 1
        , width <| px <| floor (68 * scale)
        , height <| px <| floor (105 * scale)
        ]
        [ row
            [ padding <| floor (3 * scale)
            , width fill
            , Font.size <| floor (20 * scale)
            , spacing <| floor (3 * scale)
            , Border.roundEach
                { topLeft = round
                , topRight = round
                , bottomLeft = 0
                , bottomRight = 0
                }
            , Background.color <| Tuple.second <| suitOutput card.suit
            ]
            [ viewRank card.rank
            , el [ Font.color <| rgb 1 1 1 ] <|
                text <|
                    Tuple.first <|
                        suitOutput card.suit
            ]
        , el
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
        ]


viewRank : Rank -> Element msg
viewRank rank =
    el [ Font.color <| rgb 1 1 1 ] <|
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
