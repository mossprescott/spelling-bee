-- Non-application previews for use with `elm reactor`.


module Demo.FrozenToday exposing (..)

import Array
import Bee exposing (Model)
import Demo.FrozenMain exposing (Msg, frozenMain)
import Dict
import Puzzle exposing (..)
import Views exposing (Size, WordListSortOrder(..))


main : Program () Model Msg
main =
    frozenMain startModel


startModel : Model
startModel =
    { data =
        Just
            { user = Just "jeff"
            , id = 1234
            , nextPuzzleId = Nothing
            , previousPuzzleId = Just 1233
            , puzzle =
                { expiration = Just 1614326400
                , displayWeekday = "Thursday"
                , displayDate = "February 25, 2021"
                , printDate = "2021-02-25"
                , editor = "Sam Ezersky"
                , centerLetter = 'o'
                , outerLetters = [ 'a', 'g', 'l', 'm', 'r', 'u' ]
                }
            , found =
                [ ( "glom", [ "jeff", "steve" ] )
                , ( "gloom", [ "jeff" ] )
                , ( "amoral", [ "jeff" ] )
                ]
            , hints =
                { maxScore = 150
                }
            , friends =
                Dict.fromList
                    [ ( "steve", { score = 120, hasPangram = True } )
                    , ( "jeff", { score = 6, hasPangram = False } )
                    , ( "dave", { score = 0, hasPangram = False } )
                    ]
            , group = { score = 121, hasAllPangrams = False }
            }
    , letters = Array.fromList [ 'a', 'g', 'l', 'o', 'm', 'r', 'u' ]
    , input = [ 'l', 'o', 'a', 'm' ]
    , selectedPuzzleId = Just 1234
    , message = Nothing
    , wordSort = Alpha
    , viewport = Size 375 675
    }
