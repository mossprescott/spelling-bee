module Demo.Local exposing (..)

import Bee exposing (Model, Msg, beeMain)
import Puzzle exposing (herokuBaseUrl, webBackend)


{-| This saves having to manually edit baseUrl to run locally against the actual server.

Note: there's no easy way to get this to connect as an authenticated user, so you only get
to test "Guest" mode.

-}
main : Program () Model Msg
main =
    beeMain <| webBackend herokuBaseUrl
