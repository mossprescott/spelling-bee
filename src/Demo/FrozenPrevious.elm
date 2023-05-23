-- Non-application previews for use with `elm reactor`.


module Demo.FrozenPrevious exposing (..)

import Array
import Bee exposing (Message(..), Model)
import Demo.FrozenMain exposing (Msg, frozenMain)
import Dict
import Language exposing (Language(..))
import Puzzle exposing (..)
import Views exposing (Size)
import Views.Constants exposing (ColorMode(..), WordListSortOrder(..))


main : Program () Model Msg
main =
    frozenMain startModel


startModel : Model
startModel =
    { data =
        Just
            { user = Just "jeff"
            , id = 1234
            , nextPuzzleId = Just 1235
            , previousPuzzleId = Just 1233
            , puzzle =
                { expiration = Nothing
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
                , ( "moral", [ "steve" ] )
                , ( "amoral", [ "steve" ] )
                , ( "aglomural", [] )
                ]
            , hints =
                { maxScore = 150
                , pangramCount = 1
                }
            , friends =
                Dict.fromList
                    [ ( "steve", { score = 120, hasPangram = True, hasAllPangrams = True } )
                    , ( "jeff", { score = 6, hasPangram = True, hasAllPangrams = False } )
                    , ( "dave", { score = 0, hasPangram = False, hasAllPangrams = False } )
                    ]
            , group = { score = 121, hasAllPangrams = True }
            }
    , letters = Array.fromList [ 'a', 'g', 'l', 'o', 'm', 'r', 'u' ]
    , input = [ 'l', 'o', 'a', 'm' ]
    , selectedPuzzleId = Just 1234
    , message = None
    , wordSort = Alpha
    , viewport = Size 375 675
    , colorMode = Night
    , language = ES
    }
