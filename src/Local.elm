module Local exposing (..)

import Bee exposing (Model, Msg, beeMain)
import Puzzle exposing (herokuBaseUrl, webBackend)


main : Program () Model Msg
main =
    beeMain <| webBackend herokuBaseUrl
