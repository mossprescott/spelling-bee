module Views.Permutation exposing
    ( Permutation
    , choose
    , definitely
    , get
    , init
    , moveToHead
    , rotate
    , shuffle
    , swap
    , toList
    )

{-| Non-empty collection that supports _only_ reordering operations.
-}

import Array exposing (Array)
import Dict exposing (values)
import Random exposing (Generator)
import Random.Array
import Random.List


{-| A collection of values which supports only re-ordering.

  - there are always at least 2 values (otherwise ordering is meaningless)
  - each value always appears exactly once after any sequence of operations
  - therefore, the length is always the same

Note: it's assumed that the values are distinct; if there are any duplicates,
it might or might not behave correctly.

-}
type Permutation a
    = Permutation { values : Array a, saved : a }


{-| Make a Permutation from at least two elements. Note the awkward signature: you have to
provide the first two, and then as many more as you like in a list.
-}
init : a -> a -> List a -> Permutation a
init first second rest =
    Permutation { values = Array.fromList (first :: second :: rest), saved = first }


{-| Extract the values in their current order.
-}
toList : Permutation a -> List a
toList (Permutation p) =
    Array.toList p.values


{-| Look up an element by index, treating the index as modulo the number of values.
-}
get : Int -> Permutation a -> a
get idx (Permutation p) =
    safeGet (modBy (Array.length p.values) idx) p


{-| Swap two values by index. Indexes are treated as modulo the number of values.

    >>> p = init "a" "b" [ "c", "d"]
    >>> swap 0 -1 p |> toList
    [ "d", "b", "c", "a" ]

-}
swap : Int -> Int -> Permutation a -> Permutation a
swap idx1 idx2 (Permutation p) =
    let
        fixIndex =
            modBy (Array.length p.values)

        fixed1 =
            fixIndex idx1

        fixed2 =
            fixIndex idx2
    in
    Permutation
        { values =
            p.values
                |> Array.set fixed1 (safeGet fixed2 p)
                |> Array.set fixed2 (safeGet fixed1 p)
        , saved = p.saved
        }


{-| Rotate one or more values so that each value takes the position of the previous one
(including wrapping around).
-}
rotate : List Int -> Permutation a -> Permutation a
rotate idxs perm =
    case idxs of
        firstIdx :: rest ->
            let
                loop : List Int -> Permutation a -> Permutation a
                loop is p =
                    case is of
                        i :: j :: more ->
                            loop (j :: more) (swap i j p)

                        [ lastIdx ] ->
                            swap lastIdx firstIdx p

                        [] ->
                            -- Degenerate case, but this happens if the input has just one index
                            p
            in
            loop rest perm

        [] ->
            perm


{-| Move the provided value to the first position. If the value wasn't already present, the
collection is unchanged.
-}
moveToHead : a -> Permutation a -> Permutation a
moveToHead x (Permutation p) =
    let
        others =
            Array.filter ((/=) x) p.values
    in
    if Array.length others == Array.length p.values then
        Permutation p

    else
        Permutation <|
            { values = Array.append (Array.fromList [ x ]) others
            , saved = p.saved
            }



-- Random


{-| A completely random re-ordering of the values.
-}
shuffle : Permutation a -> Generator (Permutation a)
shuffle (Permutation p) =
    Random.Array.shuffle p.values
        |> Random.map (\vals -> Permutation { values = vals, saved = p.saved })


{-| Apply a generator, based on a preceding value; if the result is the same value,
re-apply the generator until a new value is produced.
-}
definitely : (a -> Generator a) -> a -> Generator a
definitely gen x =
    gen x
        |> Random.andThen
            (\x1 ->
                if x1 == x then
                    definitely gen x

                else
                    Random.constant x1
            )


{-| Select several values at random (and in random order), from a list of possible values.
See `Random.uniform`. If the list of possible values contains less than the number of
requested values, the result contains them all.
-}
choose : Int -> List a -> Generator (List a)
choose count values =
    Random.List.shuffle values
        |> Random.map (List.take count)



-- Internal


safeGet : Int -> { values : Array a, saved : a } -> a
safeGet idx { values, saved } =
    values |> Array.get idx |> Maybe.withDefault saved
