-- Non-application previews for use with `elm reactor`.


module Demo.FrozenMain exposing (Msg, frozenMain)

import Bee exposing (Model, beeView)
import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Puzzle exposing (..)
import Task
import Views exposing (Size, WordListSortOrder(..))


frozenMain : Model -> Program () Model Msg
frozenMain startModel =
    Browser.element
        { init = always ( startModel, Task.perform (\vp -> ViewportMsg (Size (round vp.viewport.width) (round vp.viewport.height))) Browser.Dom.getViewport )
        , update = update
        , view = view
        , subscriptions = always <| Browser.Events.onResize (\w h -> ViewportMsg (Size w h))
        }


type Msg
    = ViewportMsg Size
    | Ignore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ViewportMsg size ->
            ( { model | viewport = size }, Cmd.none )

        Ignore ->
            ( model, Cmd.none )


{-| Use the full page view from the app, but ignore any messages it produces.
-}
view : Model -> Html Msg
view model =
    Html.map (always Ignore) <| beeView model
