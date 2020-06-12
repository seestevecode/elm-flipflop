module Board exposing
    ( Board
    , MoveMsg(..)
    , Tableau
    , addCardsFromStock
    , checkTableauColumnLength
    , columnWarningAtts
    , getTableauColumn
    , moveSprToFnd
    , moveSprToTab
    , moveTabToFnd
    , moveTabToTab
    , selectFromCardInTableau
    , tableauColumn
    , turnUpEndCards
    , validSprToFnd
    , validSprToTab
    , validTabToFnd
    , validTabToTab
    )

import Card exposing (Card)
import Constants as Const
import Dict exposing (Dict)
import Element exposing (Attribute)
import Element.Border as Border
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


validSprToTab : Board -> Card -> Int -> Bool
validSprToTab board card toCol =
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


validTabToTab : Board -> List Card -> Int -> Int -> ( Bool, Maybe Bool )
validTabToTab board cards fromCol toCol =
    let
        destination =
            ListX.last <| getTableauColumn board.tableau toCol
    in
    if checkTableauColumnLength board cards toCol then
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

    else
        ( False, Nothing )


validSprToFnd : Board -> Card -> Int -> Bool
validSprToFnd board card toFnd =
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


validTabToFnd : Board -> List Card -> Int -> Int -> Bool
validTabToFnd board cards fromTab toFnd =
    let
        destination =
            board.foundations
                |> ListX.getAt toFnd
                |> Maybe.withDefault []
                |> ListX.last
    in
    case ( ListX.last cards, destination ) of
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


moveSprToTab : Board -> Card -> Int -> Board
moveSprToTab board card toCol =
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


moveTabToTab : Board -> List Card -> Int -> Int -> Board
moveTabToTab board cards fromCol toCol =
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


moveSprToFnd : Board -> Card -> Int -> Board
moveSprToFnd board card toFnd =
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


moveTabToFnd : Board -> List Card -> Int -> Int -> Board
moveTabToFnd board cards fromTab toFnd =
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


columnWarningAtts : List Card -> List (Attribute msg)
columnWarningAtts cards =
    if List.length cards >= 20 then
        [ Border.color Const.columnWarningColour
        , Border.widthEach
            { bottom = Const.cardCornerRound, top = 0, right = 0, left = 0 }
        , Border.solid
        ]

    else
        []
