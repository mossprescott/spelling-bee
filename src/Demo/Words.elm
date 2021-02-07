-- Non-application previews for use with `elm reactor`.


module Demo.Words exposing (..)

-- import Puzzle exposing (UserInfo)

import Browser
import Dict
import Element exposing (column, el, none, padding, spacing)
import Html exposing (Html)
import Views exposing (WordEntry, WordListSortOrder(..), wordList)
import Views.Constants exposing (..)


main =
    Browser.sandbox
        { init = Model Alpha
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
        []
        (column
            [ spacing 10
            , padding 10
            ]
            [ wordList Alpha
                Resort
                3
                (Dict.fromList
                    [ ( "foo", WordEntry True [] )
                    ]
                )
                False
            , wordList Alpha
                Resort
                3
                (Dict.fromList
                    [ ( "foo", WordEntry True [] )
                    , ( "bar", WordEntry False [ friend1, friend2 ] )
                    ]
                )
                True
            , wordList Alpha
                Resort
                3
                (Dict.fromList
                    [ ( "foo", WordEntry True [] )
                    , ( "bar", WordEntry True [] )
                    , ( "baz", WordEntry True [] )
                    ]
                )
                False
            , wordList Alpha
                Resort
                3
                (Dict.fromList
                    [ ( "foo", WordEntry True [] )
                    , ( "bar", WordEntry True [] )
                    , ( "baz", WordEntry True [] )
                    , ( "quux", WordEntry True [] )
                    , ( "schlamozzle", WordEntry True [] )
                    ]
                )
                False
            , wordList Alpha
                Resort
                3
                (Dict.fromList
                    [ ( "foo", WordEntry True [] )
                    , ( "bar", WordEntry True [] )
                    , ( "baz", WordEntry True [] )
                    , ( "quux", WordEntry True [] )
                    , ( "schlamozzle", WordEntry True [] )
                    , ( "nerp", WordEntry True [] )
                    , ( "shazbot", WordEntry True [] )
                    ]
                )
                False
            , el [ padding 10 ] none
            , wordList
                model.dynamicSortOrder
                Resort
                3
                (Dict.fromList
                    [ ( "fool", WordEntry True [] )
                    , ( "barb", WordEntry True [] )
                    , ( "baze", WordEntry True [] )
                    , ( "quux", WordEntry True [] )
                    , ( "schlamozzle", WordEntry True [] )
                    , ( "nerp", WordEntry True [] )
                    , ( "shazbot", WordEntry True [] )
                    , ( "flop", WordEntry True [] )
                    , ( "barf", WordEntry True [] )
                    , ( "calf", WordEntry True [] )
                    , ( "calve", WordEntry True [] )
                    , ( "shave", WordEntry True [] )
                    , ( "quit", WordEntry True [] )
                    , ( "quiz", WordEntry True [] )
                    , ( "quizzing", WordEntry True [] )
                    , ( "quizical", WordEntry True [] )
                    , ( "call", WordEntry True [] )
                    , ( "shall", WordEntry True [] )
                    , ( "colt", WordEntry True [] )
                    , ( "boll", WordEntry True [] )
                    , ( "bolt", WordEntry True [] )
                    , ( "nerf", WordEntry True [] )
                    ]
                )
                False
            ]
        )


friend1 =
    ( "S", redColor )


friend2 =
    ( "D", purpleColor )
