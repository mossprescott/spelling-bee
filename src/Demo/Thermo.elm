-- Non-application previews for use with `elm reactor`.


module Demo.Thermo exposing (..)

import Browser
import Dict
import Element
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Puzzle exposing (UserInfo)
import Views exposing (friendList, scoreBanner)
import Views.Constants exposing (..)


main =
    Browser.sandbox
        { init = Day
        , update = update
        , view = view
        }


type Msg
    = SetColorMode ColorMode


type alias Model =
    ColorMode


update : Msg -> Model -> Model
update msg _ =
    case msg of
        SetColorMode mode ->
            mode


view : ColorMode -> Html Msg
view mode =
    let
        colors =
            themeColors mode
    in
    Element.layout
        [ Background.color colors.background
        , Font.color colors.foreground
        ]
        (Element.column
            [ Element.spacing 10
            , Element.padding 10
            ]
            [ scoreBanner colors 100 0 False -- Beginner (0)
            , scoreBanner colors 100 43 False -- Great
            , scoreBanner colors 100 50 True -- Amazing
            , scoreBanner colors 100 72 False -- Genius (+2)
            , scoreBanner colors 100 100 True -- Queen Bee
            , Element.el [ Element.padding 20 ] Element.none
            , friendList
                colors
                "Steve"
                (Dict.fromList
                    [ ( "Steve", UserInfo 72 True True )
                    , ( "Dave", UserInfo 0 False False )
                    , ( "Jeff", UserInfo 57 True False )
                    ]
                )
                (Dict.fromList
                    [ ( "Dave", ( colors.friends 1, 0 ) )
                    , ( "Jeff", ( colors.friends 0, 3 ) )
                    ]
                )
                100
                87
                False
            , button [ Element.padding 20 ]
                { onPress = Just <| SetColorMode <| rotate mode
                , label =
                    Element.text
                        (if mode == Day then
                            "☀"

                         else
                            "☾"
                        )
                }
            ]
        )
