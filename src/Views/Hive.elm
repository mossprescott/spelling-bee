module Views.Hive exposing
    ( Position
    , PositionState
    , animateMove
    , animator
    , atCenter
    , hive
    , startPositions
    )

{-| Relative position of a letter, in terms of rows and colums.
-}

import Animator exposing (Timeline, arriveSmoothly, at, leaveLate, leaveSmoothly, upcoming)
import Animator.Css exposing (center, transform, xy)
import Array exposing (Array)
import Element exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Set exposing (Set)
import Views.Constants exposing (Colors)


type Position
    = Center
    | Outer Int -- 0 at the top-left; clockwise after that
    | Between Position Position -- Midpoint of the animation from one position to another


{-| Where the rest of the letters start.
-}
outerPositions : List Position
outerPositions =
    List.range 0 6 |> List.map Outer


coords : Float -> Position -> { x : Float, y : Float }
coords scale pos =
    case pos of
        Center ->
            { x = 0, y = 0 }

        Outer index ->
            Array.fromList
                [ { x = -0.5 * scale, y = -1 * scale }
                , { x = 0.5 * scale, y = -1 * scale }
                , { x = -1 * scale, y = 0 * scale }
                , { x = 1 * scale, y = 0 * scale }
                , { x = -0.5 * scale, y = 1 * scale }
                , { x = 0.5 * scale, y = 1 * scale }
                ]
                -- mod?
                |> Array.get index
                -- bogus
                |> Maybe.withDefault { x = -0.5 * scale, y = -1 * scale }

        Between p1 p2 ->
            let
                c1 =
                    coords scale p1

                c2 =
                    coords scale p2

                offsetRatio =
                    0
            in
            -- Start with the midpoint, then offset perpendicularly:
            { x = ((c1.x + c2.x) / 2) + ((c2.y - c1.y) * offsetRatio)
            , y = ((c1.y + c2.y) / 2) - ((c2.x - c1.x) * offsetRatio)
            }


type alias PositionState =
    Array (Timeline Position)


startPositions : PositionState
startPositions =
    (Center :: outerPositions)
        |> List.map Animator.init
        |> Array.fromList


animator : (model -> PositionState) -> (PositionState -> model -> model) -> Animator.Animator model
animator getState setState =
    let
        watch : Int -> Animator.Animator model -> Animator.Animator model
        watch idx =
            let
                -- getter: assume idx is valid, otherwise just return a meaningless value
                get : model -> Timeline Position
                get model =
                    getState model
                        |> Array.get idx
                        |> Maybe.withDefault (Animator.init Center)

                -- setter: assume idx is valid, otherwise the value is discarded
                set : Timeline Position -> model -> model
                set newLetters model =
                    getState model
                        |> Array.set idx newLetters
                        |> (\s -> setState s model)
            in
            Animator.Css.watching get set
    in
    List.foldl watch Animator.animator (List.range 0 6)


destination : Position -> Position
destination pos =
    case pos of
        Between _ new ->
            new

        _ ->
            pos


animateMove : Position -> Position -> Timeline Position -> Timeline Position
animateMove old new =
    Animator.interrupt
        [ Animator.event Animator.quickly (Between (destination old) (destination new))
        , Animator.event Animator.quickly (destination new)
        ]


atCenter : Timeline Position -> Bool
atCenter =
    upcoming Center


hive : Colors -> Char -> List ( Char, Timeline Position ) -> Set Char -> Element Char
hive colors center letters used =
    let
        slide =
            True

        fade =
            True

        scale =
            65

        -- xy transform seems to be some fixed curve:
        position : Maybe (Animator.Css.Attribute Position)
        position =
            if slide then
                Just (transform (coords scale >> xy))

            else
                Nothing

        -- layer a fade on top of the slide:
        visibility : Maybe (Animator.Css.Attribute Position)
        visibility =
            if fade then
                Just <|
                    Animator.Css.opacity <|
                        \pos ->
                            case pos of
                                Between _ _ ->
                                    -- Fade out very quickly, to just suggest the destination:
                                    at 0 |> leaveSmoothly 0 |> arriveSmoothly 0.7

                                _ ->
                                    -- Fade in very late:
                                    at 1 |> leaveSmoothly 0.7 |> arriveSmoothly 0

            else
                Nothing

        accentColor letter =
            if letter == center then
                colors.primaryTint

            else
                colors.secondaryTint

        geometryStyle pos =
            let
                c =
                    coords scale pos
            in
            [ Html.Attributes.style "width" "54px"
            , Html.Attributes.style "height" "54px"
            , Html.Attributes.style "position" "absolute"
            ]
                ++ (if slide then
                        [ Html.Attributes.style "left" "65px"
                        , Html.Attributes.style "top" "65px"
                        ]

                    else
                        [ Html.Attributes.style "left" (String.fromFloat (65 + c.x) ++ "px")
                        , Html.Attributes.style "top" (String.fromFloat (65 + c.y) ++ "px")
                        ]
                   )

        accentStyle letter =
            if Set.member letter used then
                [ Html.Attributes.style "background-color" (toCssColor <| colors.background)
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
                (List.filterMap identity [ position, visibility ])
                (geometryStyle (destination <| Animator.current pos)
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



-- Utilities


toCssColor : Color -> String
toCssColor color =
    let
        rgb =
            toRgb color
    in
    "rgb(" ++ (String.join " " <| List.map (\x -> String.fromInt (round (255 * x))) [ rgb.red, rgb.green, rgb.blue ]) ++ ")"
