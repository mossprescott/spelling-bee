module Views.Thermo exposing (ThermoStyle, scoreRating, scoreThermo)

import Array exposing (Array)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


type alias ThermoStyle =
    { unfilled : Color -- e.g. grey
    , filled : Color -- e.g. blue
    , maxed : Color -- e.g. yellow for Queen Bee
    , labelSize : Int
    , bigRadius : Int
    , smallRadius : Int
    , connectorWidth : Int
    , showScore : Bool
    , showNext : Bool
    }


{-| Displays a single score value visually as a "thermometer" which fills in from the left,
comparing the score to a set of threshold values, which are derived from the maximum
possible score. When "hasBonus" is true (i.e. the player has a pangram), any large bubbles
are filled with solid color.
-}
scoreThermo : ThermoStyle -> Int -> Int -> Bool -> Element msg
scoreThermo style maxScore score hasBonus =
    let
        connector =
            el
                [ paddingXY style.connectorWidth 1
                , Background.color style.unfilled
                ]
                none
    in
    row
        [ Font.size style.labelSize
        ]
        (List.intersperse connector <|
            List.map renderBubble <|
                scoreBubbles style maxScore score hasBonus
        )


{-| Score level thresholds, painstakingly reverse-engineered. Note: Queen Bee doesn't appear in
thermos (until you actually achieve it), to encourage a sense of accomplishment at Genius level.
-}
thresholds : Array ( String, Float )
thresholds =
    Array.fromList
        [ ( "Beginner", 0.0 )
        , ( "Good Start", 0.02 )
        , ( "Moving Up", 0.05 )
        , ( "Good", 0.08 )
        , ( "Solid", 0.15 )
        , ( "Nice", 0.25 )
        , ( "Great", 0.4 )
        , ( "Amazing", 0.5 )
        , ( "Genius", 0.7 )
        , ( "Queen Bee", 1.0 )
        ]


scoreRating : Int -> Int -> String
scoreRating maxScore score =
    Maybe.withDefault "" <|
        List.head <|
            List.reverse <|
                List.filterMap
                    (\( n, t ) ->
                        if score >= scaleThreshold maxScore t then
                            Just n

                        else
                            Nothing
                    )
                <|
                    Array.toList thresholds


{-| Render the line of bubbles corresponding to a score and "bonus" flag, in the context of
a certain maxScore and style.
The bonus means you have a pangram, usually.
-}
scoreBubbles : ThermoStyle -> Int -> Int -> Bool -> List Bubble
scoreBubbles style maxScore score hasBonus =
    let
        scoreLabel =
            if style.showScore then
                Just <| String.fromInt score

            else
                Nothing

        bigBlue =
            Bubble style.filled scoreLabel Circle style.bigRadius hasBonus

        bigGray t =
            if style.showNext then
                Bubble style.unfilled (Just <| "+" ++ String.fromInt (t - score)) Circle style.bigRadius False

            else
                smallGray

        smallBlue =
            Bubble style.filled Nothing Circle style.smallRadius True

        smallGray =
            Bubble style.unfilled Nothing Circle style.smallRadius True

        squared bubble =
            { bubble | shape = Square }

        maxed bubble =
            { bubble | color = style.maxed }

        start ths =
            case ths of
                [] ->
                    []

                _ :: [] ->
                    [ (if score == maxScore then
                        maxed

                       else
                        identity
                      )
                      <|
                        squared bigBlue
                    ]

                t1 :: t2 :: more ->
                    if score == 0 then
                        bigBlue
                            :: bigGray t2
                            :: after more

                    else if score >= t2 then
                        smallBlue
                            :: start (t2 :: more)

                    else if score >= t1 then
                        bigBlue
                            :: (if List.isEmpty more then
                                    squared <| bigGray t2

                                else
                                    bigGray t2
                               )
                            :: after more

                    else
                        []

        after ths =
            case ths of
                [] ->
                    []

                _ :: [] ->
                    [ squared smallGray ]

                _ :: more ->
                    smallGray :: after more
    in
    start (Array.toList <| scoreThresholds maxScore)


{-| Thresholds for the day, as integers, not including "Queen Bee/100%".
-}
scoreThresholds : Int -> Array Int
scoreThresholds maxScore =
    Array.map (scaleThreshold maxScore << Tuple.second) (Array.slice 0 -1 thresholds)


scaleThreshold : Int -> Float -> Int
scaleThreshold maxScore ratio =
    roundHalfEven (ratio * toFloat maxScore)


type alias Bubble =
    { color : Color
    , label : Maybe String
    , shape : BubbleShape
    , radius : Int
    , fill : Bool
    }


type BubbleShape
    = Circle
    | Square


renderBubble : Bubble -> Element msg
renderBubble bubble =
    el
        ([ if bubble.fill then
            Just <| Background.color bubble.color

           else
            Nothing
         , Just <| Border.color bubble.color
         , Just <| Border.width 2
         , Just <|
            Border.rounded
                (if bubble.shape == Circle then
                    bubble.radius

                 else
                    0
                )
         , Just <| width (px <| 2 * bubble.radius)
         , Just <| height (px <| 2 * bubble.radius)
         , Just <| centerY
         ]
            |> List.filterMap identity
        )
        (el
            [ centerX, centerY ]
            (text <| Maybe.withDefault "" bubble.label)
        )


{-| Round to the nearest Int, choosing the nearest _even_ value (whether up or down) when the
value is exactly half.
-}
roundHalfEven : Float -> Int
roundHalfEven x =
    let
        isInt y =
            toFloat (truncate y) == y

        isHalf =
            not (isInt x) && isInt (2 * x)

        rounded =
            Basics.round x

        isEven =
            modBy 2 rounded == 0
    in
    if isHalf && not isEven then
        rounded - 1

    else
        rounded
