module PermutationsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Set
import Test exposing (..)
import Views.Permutation as P exposing (Permutation)


{-| Note: these tests assume the values are distinct, even though the type might actually
behave correctly even if they're not. It's just easier to test thay way.
-}
testPermutation : Test
testPermutation =
    let
        digits =
            P.init 1 2 [ 3, 4, 5 ]
    in
    describe "Permutation"
        [ describe "init"
            [ test "contents" <|
                \_ ->
                    P.toList digits
                        |> Expect.equalLists [ 1, 2, 3, 4, 5 ]
            , fuzz3 Fuzz.int Fuzz.int (Fuzz.list Fuzz.int) "starts out with the right stuff" <|
                \x y rest ->
                    (P.init x y rest |> P.toList)
                        |> Expect.equalLists (x :: y :: rest)
            ]
        , describe "swap" <|
            [ fuzz2 (Fuzz.intRange 0 4) (Fuzz.intRange 0 4) "correctly swaps with expected indices (x -> y)" <|
                \idx1 idx2 ->
                    let
                        swapped =
                            digits |> P.swap idx1 idx2

                        old1 =
                            digits |> at idx1

                        new2 =
                            swapped |> at idx2
                    in
                    new2 |> Expect.equal old1
            , fuzz2 (Fuzz.intRange 0 4) (Fuzz.intRange 0 4) "correctly swaps with expected indices (x <- y)" <|
                \idx1 idx2 ->
                    let
                        swapped =
                            digits |> P.swap idx1 idx2

                        old2 =
                            digits |> at idx2

                        new1 =
                            swapped |> at idx1
                    in
                    new1 |> Expect.equal old2
            , fuzz2 Fuzz.int Fuzz.int "retains values" <|
                \idx1 idx2 ->
                    P.swap idx1 idx2 digits
                        |> sameValues digits
            ]
        , describe "moveToHead"
            [ fuzz (Fuzz.oneOfValues <| P.toList digits) "moves the requested value" <|
                \val ->
                    P.moveToHead val digits
                        |> P.toList
                        |> List.head
                        |> Expect.equal (Just val)
            , fuzz (Fuzz.oneOfValues <| P.toList digits) "retains values" <|
                \val ->
                    P.moveToHead val digits
                        |> sameValues digits
            , fuzz (Fuzz.intRange 10 10) "ignores any other value" <|
                \badVal ->
                    P.moveToHead badVal digits
                        |> sameOrder digits
            ]
        ]


at : Int -> Permutation a -> Maybe a
at idx perm =
    perm |> P.toList |> List.drop idx |> List.head



-- noDupes : Permutation Int -> Expectation
-- noDupes perm =
--     let
--         vals =
--             perm |> P.toList
--         uniqueVals =
--             vals |> Set.fromList
--     in
--     Set.size uniqueVals
--         |> Expect.equal (List.length vals)


sameValues : Permutation comparable -> Permutation comparable -> Expectation
sameValues p1 p2 =
    (p1 |> P.toList |> Set.fromList)
        |> Expect.equalSets (p2 |> P.toList |> Set.fromList)


sameOrder : Permutation comparable -> Permutation comparable -> Expectation
sameOrder p1 p2 =
    (p1 |> P.toList)
        |> Expect.equalLists (p2 |> P.toList)
