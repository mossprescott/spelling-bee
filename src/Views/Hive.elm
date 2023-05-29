module Views.Hive exposing
    ( Position
    , PositionState
    , ShuffleOp
    , animator
    , applyPositions
    , centerAtCenter
    , currentPositions
    , hive
    , shuffle
    , startPositions
    )

{-| Relative position of a letter, in terms of rows and colums.
-}

import Animator exposing (Animator, Timeline, arriveEarly, arriveSmoothly, at, leaveSmoothly, upcomingWith)
import Animator.Css exposing (center, transform, xy)
import Array
import Element exposing (..)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random exposing (Generator)
import Set exposing (Set)
import Views.Constants exposing (Colors)
import Views.Permutation as Permutation exposing (Permutation)


options =
    { -- Letters slide (linearly) from old to new position
      slide = True

    -- Letters fade out quickly, then back in quickly ()
    , fade =
        True

    -- How much of the time is in the "faded" phase, or something like that
    , smoothness = 0.8

    -- A very slight adjustment for each animating letter to look a little less robotic.
    -- Disabled for now because it's not very effective, applying as it does only to the
    -- fade and not the more noticable slide.
    , stagger = 0.0

    -- The time for *half* of the animation:
    , speed = Animator.veryQuickly
    }


{-| Possible positions for a letter, ignoring animation states.
-}
type Position
    = Center
    | Outer Int -- 0 at the top-left; clockwise after that


{-| Possible positions for a letter, including the halfway state of animating a letter to a new location.
-}
type DynamicPosition
    = LetterAt Position
    | LetterBetween Position Position



-- | Between Position Position -- Midpoint of the animation from one position to another


{-| Where the rest of the letters start.
-}
outerPositions : List Position
outerPositions =
    List.range 0 6 |> List.map Outer


coords : Float -> DynamicPosition -> { x : Float, y : Float }
coords scale pos =
    case pos of
        LetterAt Center ->
            { x = 0, y = 0 }

        LetterAt (Outer index) ->
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

        LetterBetween p1 p2 ->
            let
                c1 =
                    coords scale (LetterAt p1)

                c2 =
                    coords scale (LetterAt p2)

                offsetRatio =
                    -0.1
            in
            -- Start with the midpoint, then offset perpendicularly:
            { x = ((c1.x + c2.x) / 2) + ((c2.y - c1.y) * offsetRatio)
            , y = ((c1.y + c2.y) / 2) + ((c2.x - c1.x) * offsetRatio)
            }


{-| State of all of the letters, which is a permutation of the sequence of positions, which
may be in a state of anuimation from one sequence to the next.
-}
type PositionState
    = At (Permutation Position)
    | Between (Permutation Position) (Permutation Position)


startPositions : PositionState
startPositions =
    At (Permutation.init Center (Outer 0) (List.drop 1 outerPositions))


animator : (model -> Timeline PositionState) -> (Timeline PositionState -> model -> model) -> Animator model
animator getState setState =
    Animator.animator
        |> Animator.Css.watching
            getState
            setState



-- Shuffling


centerAtCenter : Timeline PositionState -> Bool
centerAtCenter =
    upcomingWith (stateToPerm >> Permutation.toList >> List.head >> Maybe.map ((==) Center) >> Maybe.withDefault True)


stateToPerm : PositionState -> Permutation Position
stateToPerm state =
    case state of
        At pos ->
            pos

        Between _ dst ->
            dst


{-| Operations you can select from to influence what kind of re-arrangements happen, and how often.

Note: each individual swap can randomly swap a letter with itself, and subsequent swaps can
affect the same letters, so the overall effect is that fewer letters are actually moved than
op.swaps would seem to call for.

-}
type alias ShuffleOp =
    { swaps : Int
    , restoreCenter : Bool
    }


shuffle : ShuffleOp -> Permutation Position -> Generator (Permutation Position)
shuffle op =
    let
        -- Note: a single swap involves 2 digits, and so on. And if somebody asks for
        -- zero swaps, that would be an infinite loop, so just assume they meant one.
        numDigits =
            if op.swaps < 1 then
                2

            else
                op.swaps + 1

        doSwaps : Permutation Position -> Generator (Permutation Position)
        doSwaps state =
            Permutation.choose numDigits (List.range 0 6)
                |> Random.map (\idxs -> Permutation.rotate idxs state)

        doRestore : Permutation Position -> Permutation Position
        doRestore =
            if op.restoreCenter then
                Permutation.moveToHead Center

            else
                identity
    in
    Permutation.definitely (doSwaps >> Random.map doRestore)


{-| Based on the current state, possibly in the middle of animation, get the positions of each
letter, as they will be when any animation is complete.
-}
currentPositions : Timeline PositionState -> Permutation Position
currentPositions =
    Animator.current >> stateToPerm


{-| Start animating the letters to a set of new positions.
-}
applyPositions : Permutation Position -> Timeline PositionState -> Timeline PositionState
applyPositions newPositions state =
    state
        |> Animator.interrupt
            [ Animator.event options.speed (Between (currentPositions state) newPositions)
            , Animator.event options.speed (At newPositions)
            ]



-- View


hive : Colors -> Char -> List Char -> Timeline PositionState -> Set Char -> Element Char
hive colors center letters state used =
    let
        scale =
            65

        letterState : Int -> PositionState -> DynamicPosition
        letterState idx ps =
            case ps of
                At pos ->
                    LetterAt
                        (Permutation.get idx pos)

                Between src dst ->
                    let
                        p1 =
                            Permutation.get idx src

                        p2 =
                            Permutation.get idx dst
                    in
                    if p1 == p2 then
                        LetterAt p2

                    else
                        LetterBetween p1 p2

        -- xy transform seems to be some fixed curve:
        position : Int -> Maybe (Animator.Css.Attribute PositionState)
        position idx =
            if options.slide then
                Just (transform (letterState idx >> coords scale >> xy))

            else
                Nothing

        -- layer a fade on top of the slide:
        visibility : Int -> Maybe (Animator.Css.Attribute PositionState)
        visibility idx =
            -- let
            --     foo : PositionState -> Animator.Movement
            --     foo ps =
            -- in
            if options.fade then
                Just <|
                    Animator.Css.opacity <|
                        \ps ->
                            case letterState idx ps of
                                LetterBetween _ _ ->
                                    -- Fade out very quickly, to just suggest the destination:
                                    at 0 |> leaveSmoothly 0 |> arriveSmoothly options.smoothness

                                LetterAt _ ->
                                    -- Fade in very late:
                                    at 1
                                        |> leaveSmoothly options.smoothness
                                        |> arriveSmoothly 0
                                        |> arriveEarly (options.stagger * toFloat idx)

            else
                Nothing

        accentColor letter =
            if letter == center then
                colors.primaryTint

            else
                colors.secondaryTint

        geometryStyle : Position -> List (Html.Attribute msg)
        geometryStyle pos =
            let
                c =
                    coords scale (LetterAt pos)
            in
            [ Html.Attributes.style "width" "54px"
            , Html.Attributes.style "height" "54px"
            , Html.Attributes.style "position" "absolute"
            ]
                ++ (if options.slide then
                        [ Html.Attributes.style "left" "65px"
                        , Html.Attributes.style "top" "65px"
                        ]

                    else
                        [ Html.Attributes.style "left" (String.fromFloat (65 + c.x) ++ "px")
                        , Html.Attributes.style "top" (String.fromFloat (65 + c.y) ++ "px")
                        ]
                   )

        fixedAppearanceStyle =
            [ Html.Attributes.style "font-size" "32px"
            , Html.Attributes.style "border-radius" "5px"
            , Html.Attributes.style "border-style" "solid"
            , Html.Attributes.style "border-width" "3px"
            ]

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

        cell : Int -> Char -> Html Char
        cell idx letter =
            Animator.Css.div state
                (List.filterMap identity [ position idx, visibility idx ])
                (geometryStyle (currentPositions state |> Permutation.get idx)
                    ++ (fixedAppearanceStyle
                            ++ accentStyle letter
                            ++ centerStyle
                            ++ buttonStyle letter
                       )
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
                -- Note: stagger is applied to all letters, animating or not. Which is currently
                -- moot anyway.
                List.indexedMap cell letters



-- Utilities


toCssColor : Color -> String
toCssColor color =
    let
        rgb =
            toRgb color
    in
    "rgb(" ++ (String.join " " <| List.map (\x -> String.fromInt (round (255 * x))) [ rgb.red, rgb.green, rgb.blue ]) ++ ")"
