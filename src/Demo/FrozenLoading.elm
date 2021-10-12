-- Non-application previews for use with `elm reactor`.


module Demo.FrozenLoading exposing (..)

import Array
import Bee exposing (Model, Msg, beeView, startModel)
import Browser
import Dict
import Html exposing (Html)
import Puzzle exposing (..)
import Views exposing (WordListSortOrder(..))


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , update = always
        , view = view
        }


model : Model
model =
    startModel


view : () -> Html ()
view =
    always <| Html.map (always ()) <| beeView model
