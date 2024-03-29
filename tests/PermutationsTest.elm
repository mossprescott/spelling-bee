module PermutationsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz
import Html exposing (a)
import Random exposing (Generator)
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
        , describe "rotate"
            [ test "does nothing with empty list" <|
                \_ ->
                    let
                        rotated =
                            P.rotate [] digits
                    in
                    P.toList rotated
                        |> Expect.equal (P.toList digits)
            , fuzz (Fuzz.intRange -5 15) "does nothing with singleton" <|
                \idx ->
                    let
                        rotated =
                            P.rotate [ idx ] digits
                    in
                    P.toList rotated
                        |> Expect.equal (P.toList digits)
            , test "rotates 3 values" <|
                \_ ->
                    P.toList (P.rotate [ 0, 2, 3 ] digits)
                        |> Expect.equalLists [ 3, 2, 4, 1, 5 ]
            , fuzz (fuzzIdxs 10 10) "retains values" <|
                \idxs ->
                    P.rotate idxs digits
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
        , let
            -- A smaller list makes it more likely we'll randomly get the same order back
            smallDigits =
                P.init 1 2 [ 3 ]
          in
          describe "shuffle"
            [ fuzz Fuzz.int "retains values" <|
                \seed ->
                    let
                        shuffled =
                            run (P.shuffle smallDigits) seed
                    in
                    shuffled
                        |> sameValues smallDigits
            , fuzz Fuzz.int "always shuffles (when `definitely` is applied)" <|
                \seed ->
                    let
                        shuffled =
                            run (P.definitely P.shuffle smallDigits) seed
                    in
                    (P.toList shuffled == P.toList smallDigits)
                        |> Expect.equal False
                        |> Expect.onFail "no change after shuffle"
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


run : Generator a -> Int -> a
run gen seed =
    Random.initialSeed seed |> Random.step gen |> Tuple.first


{-| A Fuzzer for lists of indexes (as used by `Permutation.rotate`). The list will have between 0
and maxCount indexes in it, and each index will be between 0 and maxCount.
-}
fuzzIdxs : Int -> Int -> Fuzz.Fuzzer (List Int)
fuzzIdxs maxIndex maxCount =
    Fuzz.map2 (\seed count -> run (P.choose count (List.range 0 maxIndex)) seed)
        Fuzz.int
        (Fuzz.intRange 0 maxCount)
