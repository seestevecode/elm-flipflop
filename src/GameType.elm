module GameType exposing
    ( GameType
    , boardFromDeck
    , buildTableau
    , deck
    , getGameType
    , validGameTypes
    )

import Board exposing (Board)
import Card exposing (Card)
import Dict exposing (Dict)
import List.Extra as ListX


type alias GameType =
    { name : String
    , icons : String
    , numFoundations : Int
    , numSuits : Int
    , numTableauCards : Int
    , tableauColSizes : List Int
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
        |> Maybe.withDefault (GameType "1-suit" "" 4 1 25 [ 5, 5, 5, 5, 5 ])


validGameTypes : Dict Int GameType
validGameTypes =
    let
        gameTypes =
            [ GameType "One Suit" "♠♠♠♠" 4 1 25 [ 5, 5, 5, 5, 5 ]
            , GameType "Two Suit" "♠♥♠♥" 4 2 25 [ 5, 5, 5, 5, 5 ]
            , GameType "Three Suit" "♠♥♣♠" 4 3 25 [ 5, 5, 5, 5, 5 ]
            , GameType "Four Suit" "♠♥♣♦" 4 4 25 [ 5, 5, 5, 5, 5 ]
            , GameType "Five Suit" "♠♥♣♦★" 5 5 28 [ 6, 6, 6, 5, 5 ]
            , GameType "One Suit Extra" "♠♠♠♠♠" 5 1 28 [ 6, 6, 6, 5, 5 ]
            ]

        indices =
            List.range 0 (List.length gameTypes - 1)
    in
    List.map2 Tuple.pair indices gameTypes |> Dict.fromList
