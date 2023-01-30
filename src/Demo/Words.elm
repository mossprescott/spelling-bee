-- Non-application previews for use with `elm reactor`.


module Demo.Words exposing (..)

import Browser
import Element exposing (column, el, none, padding, spacing)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Views exposing (WordEntry, WordListSortOrder(..), wordList)
import Views.Constants exposing (..)


main =
    Browser.sandbox
        { init = Model Found
        , update = update
        , view = view
        }


type alias Model =
    { dynamicSortOrder : WordListSortOrder
    }


type Msg
    = Resort WordListSortOrder


update : Msg -> Model -> Model
update msg model =
    case msg of
        Resort order ->
            { model | dynamicSortOrder = order }


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color colors.background
        , Font.color colors.foreground
        ]
        (column
            [ spacing 10
            , padding 10
            ]
            [ wordList colors
                Alpha
                Resort
                3
                [ WordEntry "foo" True []
                ]
                False
            , wordList colors
                Alpha
                Resort
                3
                [ WordEntry "foo" True []
                , WordEntry "bar" False [ friend1, friend2 ]
                ]
                True
            , wordList colors
                Alpha
                Resort
                3
                [ WordEntry "foo" True []
                , WordEntry "bar" True []
                , WordEntry "baz" True []
                ]
                False
            , wordList colors
                Alpha
                Resort
                3
                [ WordEntry "foo" True []
                , WordEntry "bar" True []
                , WordEntry "baz" True []
                , WordEntry "quux" True []
                , WordEntry "schlamozzle" True []
                ]
                False
            , wordList colors
                Alpha
                Resort
                3
                [ WordEntry "foo" True []
                , WordEntry "bar" True []
                , WordEntry "baz" True []
                , WordEntry "quux" True []
                , WordEntry "schlamozzle" True []
                , WordEntry "nerp" True []
                , WordEntry "shazbot" True []
                ]
                False
            , el [ padding 10 ] none
            , wordList colors
                model.dynamicSortOrder
                Resort
                3
                [ WordEntry "fool" True []
                , WordEntry "barb" True []
                , WordEntry "baze" True []
                , WordEntry "quux" True []
                , WordEntry "schlamozzle" True []
                , WordEntry "nerp" True []
                , WordEntry "shazbot" True []
                , WordEntry "flop" True []
                , WordEntry "barf" True []
                , WordEntry "calf" True []
                , WordEntry "calve" True []
                , WordEntry "shave" True []
                , WordEntry "quit" True []
                , WordEntry "quiz" True []
                , WordEntry "quizzing" True []
                , WordEntry "quizical" True []
                , WordEntry "call" True []
                , WordEntry "shall" True []
                , WordEntry "colt" True []
                , WordEntry "boll" True []
                , WordEntry "bolt" True []
                , WordEntry "nerf" True []
                ]
                False
            ]
        )


colors =
    themeColors Night


friend1 =
    ( "S", colors.friends 1 )


friend2 =
    ( "D", colors.friends 0 )
