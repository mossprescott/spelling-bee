module PermutationsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Set
import Test exposing (..)
import Views.Permutation as P exposing (Permutation)


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
        , describe "moveToHead"
            [ fuzz (Fuzz.oneOfValues <| P.toList digits) "moves the requested value" <|
                \val ->
                    P.moveToHead val digits
                        |> P.toList
                        |> List.head
                        |> Expect.equal (Just val)
            , fuzz (Fuzz.oneOfValues <| P.toList digits) "doesn't duplicate values" <|
                \val ->
                    P.moveToHead val digits
                        |> noDupes
            , fuzz (Fuzz.oneOfValues <| P.toList digits) "doesn't lose values" <|
                \val ->
                    P.moveToHead val digits
                        |> sameValues digits
            , fuzz (Fuzz.intRange 10 10) "ignores any other value" <|
                \badVal ->
                    P.moveToHead badVal digits
                        |> sameOrder digits
            ]
        ]


noDupes : Permutation Int -> Expectation
noDupes perm =
    let
        vals =
            perm |> P.toList

        uniqueVals =
            vals |> Set.fromList
    in
    Set.size uniqueVals
        |> Expect.equal (List.length vals)


sameValues : Permutation comparable -> Permutation comparable -> Expectation
sameValues p1 p2 =
    (p1 |> P.toList |> Set.fromList)
        |> Expect.equalSets (p2 |> P.toList |> Set.fromList)


sameOrder : Permutation comparable -> Permutation comparable -> Expectation
sameOrder p1 p2 =
    (p1 |> P.toList)
        |> Expect.equalLists (p2 |> P.toList)
