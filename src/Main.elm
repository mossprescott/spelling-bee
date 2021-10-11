module Main exposing (..)

import Bee exposing (Model, Msg, beeMain)
import Puzzle exposing (relativeBaseUrl, webBackend)


main : Program () Model Msg
main =
    beeMain <| webBackend relativeBaseUrl
