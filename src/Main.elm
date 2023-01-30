module Main exposing (..)

import Bee exposing (Flags, Model, Msg, beeMain)
import Puzzle exposing (relativeBaseUrl, webBackend)


main : Program Flags Model Msg
main =
    beeMain <| webBackend relativeBaseUrl
