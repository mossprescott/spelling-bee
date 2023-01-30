-- Non-application previews for use with `elm reactor`.


module Demo.FrozenToday exposing (..)

import Array
import Bee exposing (Message(..), Model)
import Demo.FrozenMain exposing (Msg, frozenMain)
import Dict
import Puzzle exposing (..)
import Views exposing (Size, WordListSortOrder(..))
import Views.Constants exposing (ColorMode(..))


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
                , pangramCount = 2
                }
            , friends =
                Dict.fromList
                    [ ( "steve", { score = 120, hasPangram = True, hasAllPangrams = True } )
                    , ( "jeff", { score = 6, hasPangram = False, hasAllPangrams = False } )
                    , ( "dave", { score = 0, hasPangram = False, hasAllPangrams = False } )
                    ]
            , group = { score = 121, hasAllPangrams = False }
            }
    , letters = Array.fromList [ 'a', 'g', 'l', 'o', 'm', 'r', 'u' ]
    , input = [ 'g', 'l', 'a', 'm' ]
    , selectedPuzzleId = Just 1234
    , message = Warning "Missing center letter"
    , wordSort = Found
    , viewport = Size 375 675
    , colorMode = Night
    }
