module Board exposing
    ( Board
    , MoveMsg(..)
    , Tableau
    , addCardsFromStock
    , checkTableauColumnLength
    , getTableauColumn
    , moveSpareToFoundation
    , moveSpareToTableau
    , moveTableauToFoundation
    , moveTableauToTableau
    , selectFromCardInTableau
    , tableauColumn
    , turnUpEndCards
    , validateSpareToFoundation
    , validateSpareToTableau
    , validateTableauToFoundation
    , validateTableauToTableau
    )

import Card exposing (Card)
import Dict exposing (Dict)
import List.Extra as ListX


type alias Board =
    { foundations : List (List Card)
    , tableau : Tableau
    , spare : ( Maybe Card, Maybe Card )
    , stock : List (List Card)
    }


type alias Tableau =
    Dict Int (List Card)


type MoveMsg
    = MoveTableauToTableau (List Card) Int Int
    | MoveSpareToTableau Card Int
    | MoveSpareToFoundation Card Int
    | MoveTableauToFoundation (List Card) Int Int
    | MoveStockToTableau


turnUpEndCards : Tableau -> Tableau
turnUpEndCards tableau =
    tableau
        |> Dict.map
            (\_ cards ->
                case List.reverse cards of
                    [] ->
                        []

                    last :: restReversed ->
                        Card.turnUp last :: restReversed |> List.reverse
            )


addCardsFromStock : Board -> Board
addCardsFromStock board =
    { board
        | tableau =
            let
                tableauList =
                    Dict.toList board.tableau

                stockList =
                    List.take 1 board.stock |> List.concat
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


selectFromCardInTableau : Card -> Tableau -> List Card
selectFromCardInTableau card tableau =
    Dict.foldl
        (\_ column tail ->
            case tail of
                [] ->
                    Card.tailFromCard card column

                _ ->
                    tail
        )
        []
        tableau


getTableauColumn : Tableau -> Int -> List Card
getTableauColumn tableau colIndex =
    tableau
        |> Dict.filter (\k _ -> k == colIndex)
        |> Dict.values
        |> List.concat



-- Validate board moves


validateSpareToTableau : Board -> Card -> Int -> Bool
validateSpareToTableau board card toCol =
    let
        destination =
            ListX.last <| getTableauColumn board.tableau toCol
    in
    checkTableauColumnLength board [ card ] toCol
        && (case destination of
                Just d ->
                    Card.cardsLinkTableauBuild card d

                Nothing ->
                    True
           )


validateTableauToTableau :
    Board
    -> List Card
    -> Int
    -> Int
    -> ( Bool, Maybe Bool )
validateTableauToTableau board cards fromCol toCol =
    let
        destination =
            ListX.last <| getTableauColumn board.tableau toCol
    in
    case checkTableauColumnLength board cards toCol of
        True ->
            case ( List.head cards, ListX.last cards, destination ) of
                ( Just sourceHead, Just sourceLast, Just dest ) ->
                    if Card.cardsLinkTableauBuild sourceHead dest then
                        ( True, Just False )

                    else if Card.cardsLinkTableauBuild sourceLast dest then
                        ( True, Just True )

                    else
                        ( False, Nothing )

                ( _, _, Nothing ) ->
                    ( True, Just False )

                ( _, _, _ ) ->
                    ( False, Nothing )

        False ->
            ( False, Nothing )


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
            Card.cardsLinkFoundationBuild c card

        Nothing ->
            card.rank == Card.Ace


validateTableauToFoundation : Board -> List Card -> Int -> Int -> Bool
validateTableauToFoundation board cards fromTab toFnd =
    let
        sourceLast =
            ListX.last cards

        destination =
            board.foundations
                |> ListX.getAt toFnd
                |> Maybe.withDefault []
                |> ListX.last
    in
    case ( sourceLast, destination ) of
        ( Just sL, Just c ) ->
            Card.selectionValidFoundationMove (List.reverse cards)
                && Card.cardsLinkFoundationBuild c sL

        ( Just sL, Nothing ) ->
            Card.selectionValidFoundationMove (List.reverse cards)
                && (sL.rank == Card.Ace)

        ( Nothing, _ ) ->
            False


checkTableauColumnLength : Board -> List Card -> Int -> Bool
checkTableauColumnLength board cards toCol =
    List.length (getTableauColumn board.tableau toCol ++ cards) <= 20



-- Board Moves


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


moveTableauToFoundation : Board -> List Card -> Int -> Int -> Board
moveTableauToFoundation board cards fromTab toFnd =
    { board
        | tableau =
            board.tableau
                |> Dict.map
                    (\k cs ->
                        if k == fromTab then
                            cs
                                |> List.reverse
                                |> List.drop (List.length cards)
                                |> List.reverse

                        else
                            cs
                    )
                |> turnUpEndCards
        , foundations =
            board.foundations
                |> List.indexedMap
                    (\foundationIndex cs ->
                        if foundationIndex == toFnd then
                            cs ++ List.reverse cards

                        else
                            cs
                    )
    }


tableauColumn : Tableau -> Card -> Maybe Int
tableauColumn tableau card =
    tableau
        |> Dict.filter (\_ cs -> List.member card cs)
        |> Dict.keys
        |> List.head
