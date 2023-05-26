module Views.Permutation exposing
    ( Permutation
    , init
    , moveToHead
    , swap
    , toList
    )

{-| Non-empty collection that supports _only_ reordering operations.
-}

import Array exposing (Array)
import Dict exposing (values)
import Random exposing (Generator)


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


{-| A completely random re-ordering of the values.
-}
shuffle : Permutation a -> Generator (Permutation a)
shuffle perm =
    Debug.todo "shuffle"


{-| Move the provided value to the first postition. If the value wasn't already present, the
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



-- Internal


safeGet : Int -> { values : Array a, saved : a } -> a
safeGet idx { values, saved } =
    values |> Array.get idx |> Maybe.withDefault saved
