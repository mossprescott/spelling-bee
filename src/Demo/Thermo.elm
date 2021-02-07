-- Non-application previews for use with `elm reactor`.


module Demo.Thermo exposing (..)

import Browser
import Dict
import Element
import Html exposing (Html)
import Puzzle exposing (UserInfo)
import Views exposing (friendList, scoreBanner)
import Views.Constants exposing (..)


main =
    Browser.sandbox
        { init = ()
        , update = always
        , view = always view
        }


view : Html ()
view =
    Element.layout
        []
        (Element.column
            [ Element.spacing 10
            , Element.padding 10
            ]
            [ scoreBanner 100 0 -- Beginner (0)
            , scoreBanner 100 43 -- Great
            , scoreBanner 100 50 -- Amazing
            , scoreBanner 100 72 -- Genius (+2)
            , scoreBanner 100 100 -- Queen Bee
            , Element.el [ Element.padding 20 ] Element.none
            , friendList
                "Steve"
                (Dict.fromList
                    [ ( "Steve", UserInfo 72 )
                    , ( "Dave", UserInfo 0 )
                    , ( "Jeff", UserInfo 57 )
                    ]
                )
                (Dict.fromList
                    [ ( "Dave", ( "D", redColor ) )
                    , ( "Jeff", ( "J", purpleColor ) )
                    ]
                )
                100
                87
            ]
        )
