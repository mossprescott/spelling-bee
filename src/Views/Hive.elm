module Views.Hive exposing
    ( Position
    , atCenter
    , hive
    , startPositions
    )

{-| Relative position of a letter, in terms of rows and colums.
-}

import Animator exposing (Timeline)
import Animator.Css exposing (transform, xy)
import Array exposing (Array)
import Element exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Set exposing (Set)
import Views.Constants exposing (Colors)


type alias Position =
    { x : Float, y : Float }


{-| Where the "center letter" starts.
-}
centerPosition : Position
centerPosition =
    Position 0 0


{-| Where the rest of the letters start.
-}
outerPositions : List Position
outerPositions =
    [ Position -0.5 -1
    , Position 0.5 -1
    , Position -1 0
    , Position 1 0
    , Position -0.5 1
    , Position 0.5 1
    ]


startPositions : Array (Timeline Position)
startPositions =
    (centerPosition :: outerPositions)
        |> List.map Animator.init
        |> Array.fromList


atCenter : Position -> Bool
atCenter { x, y } =
    x == 0 && y == 0


hive : Colors -> Char -> List ( Char, Timeline Position ) -> Set Char -> Element Char
hive colors center letters used =
    let
        position : Animator.Css.Attribute Position
        position =
            transform <|
                \pos -> xy { x = pos.x * 65, y = pos.y * 65 }

        accentColor letter =
            if letter == center && not (Set.member letter used) then
                colors.primaryTint

            else
                colors.secondaryTint

        toCssColor : Color -> String
        toCssColor color =
            let
                rgb =
                    toRgb color
            in
            "rgb(" ++ (String.join " " <| List.map (\x -> String.fromInt (round (255 * x))) [ rgb.red, rgb.green, rgb.blue ]) ++ ")"

        geometryStyle =
            [ Html.Attributes.style "width" "54px"
            , Html.Attributes.style "height" "54px"
            , Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "left" "65px"
            , Html.Attributes.style "top" "65px"
            ]

        accentStyle letter =
            if Set.member letter used then
                [ Html.Attributes.style "border-style" "solid"
                , Html.Attributes.style "border-color" (toCssColor <| accentColor letter)
                ]

            else
                [ Html.Attributes.style "background-color" (toCssColor <| accentColor letter)
                , Html.Attributes.style "border-color" "#00000000"
                ]

        centerStyle =
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "justify-content" "center"
            ]

        buttonStyle letter =
            [ Html.Events.onClick letter
            , Html.Attributes.style "cursor" "pointer"
            ]

        cell : ( Char, Timeline Position ) -> Html Char
        cell ( letter, pos ) =
            Animator.Css.div pos
                [ position ]
                (geometryStyle
                    ++ [ Html.Attributes.style "font-size" "32px"
                       , Html.Attributes.style "border-radius" "5px"
                       , Html.Attributes.style "border-style" "solid"
                       , Html.Attributes.style "border-width" "3px"
                       ]
                    ++ accentStyle letter
                    ++ centerStyle
                    ++ buttonStyle letter
                )
                [ Html.div [] [ Html.text (String.fromChar letter) ] ]
    in
    -- Note: here we construct everything using Html rather than Element so that we can use
    -- whizzy Animator.Css. That runs more efficiently and doesn't break the debugger.
    -- Switching to raw CSS in this one corner of the code is a pain, but Element seems
    -- to handle embedding it OK with a few extra hints.
    el
        [ centerX

        -- Note: redundant size attributes to help elm-ui understand the layout
        , width <| px 190
        , height <| px 190
        ]
    <|
        html <|
            Html.div
                [ Html.Attributes.style "width" "190px"
                , Html.Attributes.style "height" "190px"
                , Html.Attributes.style "position" "relative"
                ]
            <|
                List.map cell letters
