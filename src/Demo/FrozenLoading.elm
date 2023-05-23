-- Non-application previews for use with `elm reactor`.


module Demo.FrozenLoading exposing (..)

import Bee exposing (Model, beeView, startModel)
import Browser
import Html exposing (Html)
import Puzzle exposing (..)


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , update = always
        , view = view
        }


model : Model
model =
    startModel { dark = True }


view : () -> Html ()
view =
    always <| Html.map (always ()) <| beeView model
