module Demo.FrozenFound exposing (..)

{-| Non-application previews for use with `elm reactor`.
-}

import Array
import Bee exposing (Message(..), Model)
import Demo.FrozenMain exposing (Msg, frozenMain)
import Dict
import Language exposing (Language(..))
import Puzzle exposing (..)
import Views exposing (Size)
import Views.Constants exposing (ColorMode(..), WordListSortOrder(..))
import Views.Hive exposing (startPositions)


main : Program () Model Msg
main =
    frozenMain startModel


startModel : Model
startModel =
    { data =
        Just
            { user = Just "Jeff"
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
                [ ( "glom", [ "Jeff", "Steve" ] )
                , ( "gloom", [ "Jeff" ] )
                , ( "amoral", [ "Jeff" ] )
                , ( "moral", [ "Jeff", "Steve" ] )
                ]
            , hints =
                { maxScore = 150
                , pangramCount = 2
                }
            , friends =
                Dict.fromList
                    [ ( "Steve", { score = 120, hasPangram = True, hasAllPangrams = True } )
                    , ( "Jeff", { score = 6, hasPangram = False, hasAllPangrams = False } )
                    , ( "Dave", { score = 0, hasPangram = False, hasAllPangrams = False } )
                    ]
            , group = { score = 121, hasAllPangrams = False }
            }
    , letters = startPositions
    , input = []
    , selectedPuzzleId = Just 1234
    , message = JustFound "moral"
    , wordSort = Found
    , viewport = Size 375 675
    , colorMode = Night
    , language = EN
    }
