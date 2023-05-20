module Views exposing
    ( Position
    , Size
    , WordEntry
    , assignColors
    , centerPosition
    , colorModeButton
    , controlButton
    , entered
    , friendList
    , hintFound
    , hintNone
    , hintWarning
    , hive
    , languageButton
    , loadingHeader
    , mainLayout
    , outerPositions
    , puzzleFooter
    , puzzleHeader
    , scoreBanner
    , wordList
    )

import Animator exposing (Timeline)
import Animator.Css exposing (Transform, transform, xy)
import Array exposing (Array)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Language exposing (Language(..), Strings)
import Puzzle exposing (User, UserInfo, isPangram, wordScore)
import Set exposing (Set)
import Views.Constants exposing (..)
import Views.Thermo exposing (..)


type alias Size =
    { width : Int, height : Int }


{-| Layout the page's main elements, according to the amount of available space (mainly, width).

If the view is only wide enough for one column, then everything goes in a single centered column.

If the viewport is wide enough for two columns, then the main game elements go on the right
(sorry, lefties), and everything else goes on the left. The goal is to be usable on a typical
phone in landscape, but probably not on smaller phones.

If the viewport is wide enough for three, then we must be on a large tablet or desktop,
and the game gets centered with words to the left and scores to the right, with lots of extra
space around.

Finally, some white space is added above and below, if it looks like there's more than enough
vertical space for the main game view.

`desiredColumnWidth` is an arbitrary fixed value, but corresponds to roughly the width of the
main game elements, plus a little extra space.

-}
mainLayout : Element msg -> Element msg -> Element msg -> Element msg -> Element msg -> Int -> Size -> Element msg
mainLayout header game words friends footer desiredColumnWidth actualViewport =
    let
        padYIfMoreThan ratio =
            paddingXY
                0
                (if toFloat actualViewport.height > ratio * toFloat desiredColumnWidth then
                    15

                 else
                    3
                )
    in
    if actualViewport.width < desiredColumnWidth * 2 then
        -- 1 column:
        column
            [ padYIfMoreThan 2.5
            , centerX
            , spacing 15
            ]
            [ header
            , game
            , words
            , friends
            , footer
            ]

    else if actualViewport.width < desiredColumnWidth * 3 then
        -- 2 columns:
        row
            [ padYIfMoreThan 1.3
            , centerX
            , spacing 30
            ]
            [ column
                [ alignTop
                , spacing 15
                ]
                [ header
                , words
                , friends
                , footer
                ]
            , column
                [ alignTop ]
                [ game
                ]
            ]

    else
        -- 3 columns:
        row
            [ paddingXY 15 15
            , centerX
            , spacing 30
            ]
            [ words
            , column
                [ centerX
                , spacing 25
                ]
                [ header
                , game
                , footer
                ]
            , friends
            ]


colorModeButton : Colors -> Strings -> ColorMode -> (ColorMode -> msg) -> Element msg
colorModeButton colors strings colorMode handle =
    lightweightButton colors
        (if colorMode == Day then
            "☼"

         else
            "☾"
        )
        strings.colorModeDescription
        (Just <| handle <| Views.Constants.rotate colorMode)


languageButton : Colors -> Strings -> Language -> (Language -> msg) -> Element msg
languageButton colors strings language handle =
    lightweightButton colors
        (case language of
            EN ->
                "🇺🇸"

            ES ->
                "🇲🇽"
        )
        strings.languageDescription
        (Just <| handle <| Language.rotate language)


puzzleHeader : Colors -> Strings -> String -> Maybe msg -> Maybe msg -> Element msg
puzzleHeader colors strings date previousMsg nextMsg =
    column
        [ centerX
        ]
        [ row
            [ spacing 10
            , Font.size 16
            ]
            [ lightweightButton colors "←" strings.previousPuzzleDescription previousMsg
            , lightweightButton colors "→" strings.nextPuzzleDescription nextMsg
            , el [] (text date)
            ]
        ]


loadingHeader : Strings -> Maybe String -> Element msg
loadingHeader strings msg =
    column
        [ headerFont
        , centerX
        , spacing 5
        ]
        [ el
            [ Font.bold
            , Font.size 24
            ]
            (text <| strings.titleLabel)
        , el
            [ Font.light
            , Font.size 16
            , Font.italic
            ]
            (text <| Maybe.withDefault strings.loadingLabel msg)
        ]


puzzleFooter : Colors -> Strings -> String -> Element msg
puzzleFooter colors strings editor =
    column
        [ centerX
        , spacing 5
        ]
        [ el [] (text " ") -- space
        , el
            [ Font.light
            , Font.size 16
            ]
            (text <| strings.editorLabel editor)
        , row
            [ Font.light
            , Font.size 16
            ]
            [ text <| strings.attributionLabel
            , link
                []
                { url = "https://www.nytimes.com/puzzles/spelling-bee"
                , label =
                    el [ Font.italic, mouseOver [ Font.color colors.activeHilite ] ]
                        (text <| strings.nytLabel)
                }
            ]
        , el [] (text " ") -- space
        , row
            [ Font.light
            , Font.size 16
            ]
            [ text <| strings.sourceLabel
            , link
                []
                { url = "https://github.com/mossprescott/spelling-bee"
                , label =
                    el [ Font.italic, mouseOver [ Font.color colors.activeHilite ] ]
                        (text <| strings.hereLabel)
                }
            ]
        ]


{-| View with the score "thermo" along with the name of the highest level that's been reached.
-}
scoreBanner : Colors -> Strings -> Int -> Int -> Bool -> Element msg
scoreBanner colors strings maxScore score hasAllPangrams =
    column
        [ spacing 3
        , centerX
        ]
        [ el
            [ Font.size 16
            , centerX
            ]
            (text <| strings.scoreLabel <| scoreRating maxScore score)
        , scoreThermo (mainThermoStyle colors) maxScore score hasAllPangrams
        ]


{-| Text area where letters appear (and can optionally be entered/editted directly.)
-}
entered : Colors -> (String -> msg) -> msg -> msg -> List Char -> Element msg
entered colors changedMsg enterMsg shuffleMsg chars =
    row
        [ centerX
        , onKeyStroke <| Dict.fromList [ ( "Enter", enterMsg ), ( " ", shuffleMsg ) ]
        ]
        [ Input.text
            [ Font.center
            , Background.color colors.background
            , htmlAttribute (Html.Attributes.id "input")
            ]
            { text = String.fromList chars
            , placeholder = Nothing
            , label = Input.labelHidden "Word"
            , onChange = changedMsg << String.filter ((/=) ' ')
            }
        ]


onKeyStroke : Dict String msg -> Attribute msg
onKeyStroke msgs =
    htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case Dict.get key msgs of
                            Just msg ->
                                Decode.succeed msg

                            Nothing ->
                                Decode.fail "Not a known key"
                    )
            )
        )


hintNone : Element msg
hintNone =
    el
        [ height <| px 20
        ]
        none


hintWarning : Colors -> String -> Element msg
hintWarning colors msg =
    el
        [ centerX
        , height <| px 20
        , Font.size 16
        , Font.light
        , Font.color colors.dimForeground
        ]
        (text msg)


hintFound : Colors -> WordEntry -> Element msg
hintFound colors entry =
    row
        [ centerX
        , height <| px 20
        , Font.size 16
        , Font.light
        , spacing 10
        ]
        [ word colors entry
        , text <| "+" ++ String.fromInt (wordScore entry.word)
        ]


{-| Relative position of a letter, in terms of rows and colums.
-}
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


hive : Colors -> Char -> List ( Char, Timeline Position ) -> Set Char -> Element Char
hive colors center letters used =
    let
        position : Animator.Css.Attribute Position
        position =
            transform <|
                \pos ->
                    xy { x = pos.x * 65, y = pos.y * 65 }

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


{-| Split an array of values into smaller arrays, such that the overall order is preserved,
and each array has alternating lengths. Any leftover values go in a final, possibly shorter,
array.

For example, `partition2 2 3 (Array.fromList [1, 2, 3, 4, 5, 6])` -> `[1, 2], [3, 4, 5], [6]`

-}
partition2 : Int -> Int -> Array a -> List (Array a)
partition2 x y zs =
    if Array.length zs > x then
        Array.slice 0 x zs :: partition2 y x (Array.slice x (Array.length zs) zs)

    else
        [ zs ]


{-| Actual HTML button, styled with a simple border, with a fixed width for consistent layout.
-}
controlButton : Colors -> String -> String -> msg -> Bool -> Element msg
controlButton colors label description msg enabled =
    buttonImpl
        colors
        label
        description
        (if enabled then
            Just msg

         else
            Nothing
        )
        (Just 60)


{-| HTML button, with similar style as the control buttons, but more compact.
-}
lightweightButton : Colors -> String -> String -> Maybe msg -> Element msg
lightweightButton colors label description msg =
    buttonImpl colors label description msg Nothing


buttonImpl : Colors -> String -> String -> Maybe msg -> Maybe Int -> Element msg
buttonImpl colors label description msg minWidth =
    Input.button
        [ Border.rounded 5
        , Border.color colors.activeHilite
        , Border.width 1
        , Border.solid
        , Font.light
        , Font.color
            (case msg of
                Just _ ->
                    colors.foreground

                Nothing ->
                    colors.inactiveForeground
            )
        , padding 5
        , width
            (case minWidth of
                Just w ->
                    minimum w shrink

                Nothing ->
                    shrink
            )
        , Font.center
        , Region.description description
        , noTouchDelay -- TODO: apply to container?
        ]
        { onPress = msg
        , label = text label
        }


{-| A row in the table of friends' scores, which may represent the player, a friend with their
distinguishing color and initial, or the group as a whole.
-}
type FriendEntry
    = Player User Bool
    | Friend User Color Int Bool -- note: not actually using the initial at this point
    | Group Bool


friendList : Colors -> Strings -> User -> Dict User UserInfo -> Dict User ( Color, Int ) -> Int -> Int -> Bool -> Element msg
friendList colors strings user friends decorations maxScore groupScore groupHasAllPangrams =
    let
        sortedFriends =
            friends
                |> Dict.toList
                |> List.sortBy (\( _, info ) -> -info.score)

        toEntry : User -> FriendEntry
        toEntry u =
            if u == user then
                Player user (userHasAllPangrams user)

            else
                case Dict.get u decorations of
                    Just ( color, extraScore ) ->
                        Friend u color extraScore (userHasAllPangrams u)

                    -- Note: doesn't happen if inputs are correct.
                    Nothing ->
                        Friend u colors.inactiveForeground 0 False

        userHasAllPangrams : User -> Bool
        userHasAllPangrams u =
            case Dict.get u friends of
                Just info ->
                    info.hasAllPangrams

                -- Note: doesn't happen if inputs are correct.
                Nothing ->
                    False

        -- Note: leaving out the Group entry if there are no friends (i.e. Guest mode), but
        -- include it otherwise, even if no other user has any points, because it conveys the max score.
        entries : List ( FriendEntry, Int )
        entries =
            List.map (\( u, ui ) -> ( toEntry u, ui.score )) sortedFriends
                ++ (if List.length sortedFriends > 1 then
                        [ ( Group groupHasAllPangrams, groupScore ) ]

                    else
                        []
                   )

        spacerColumn =
            { header = none
            , width = fill
            , view = always none
            }

        -- Tricky: embedding in a row gets the text vertically centered.
        -- Force alignment by matching the height of the thermo.
        centerTextCell contents =
            row
                [ height (px <| 2 * (smallThermoStyle colors colors.primaryTint).bigRadius)
                ]
                [ contents ]

        playerFontStyles entry =
            case entry of
                Player _ _ ->
                    [ Font.semiBold ]

                Friend _ _ _ _ ->
                    []

                Group _ ->
                    [ Font.semiBold, Font.italic ]
    in
    column
        [ spacing 10
        ]
        [ text <| strings.friendsLabel
        , Element.table
            [ width fill
            , spacing 9
            , centerX
            , Font.size 16
            ]
            { data = entries
            , columns =
                [ spacerColumn
                , { header = none
                  , width = shrink
                  , view =
                        \( entry, _ ) ->
                            centerTextCell <|
                                let
                                    name =
                                        case entry of
                                            Player n _ ->
                                                n

                                            Friend n _ _ _ ->
                                                n

                                            Group _ ->
                                                strings.groupLabel
                                in
                                el (playerFontStyles entry) <| text name
                  }
                , { header = none
                  , width = shrink
                  , view =
                        \( entry, score ) ->
                            case entry of
                                Player _ hasAllPangrams ->
                                    scoreThermo (smallThermoStyle colors colors.primaryTint) maxScore score hasAllPangrams

                                Friend _ color _ hasAllPangrams ->
                                    if score > 0 then
                                        scoreThermo (smallThermoStyle colors color) maxScore score hasAllPangrams

                                    else
                                        none

                                Group hasAllPangrams ->
                                    scoreThermo (smallThermoStyle colors colors.primaryTint) maxScore score hasAllPangrams
                  }
                , { header = none
                  , width = shrink
                  , view =
                        \( entry, score ) ->
                            centerTextCell <|
                                el (playerFontStyles entry ++ [ Font.size friendScoreSize ]) (text <| String.fromInt score)
                  }
                , { header = none
                  , width = shrink
                  , view =
                        \( entry, _ ) ->
                            centerTextCell <|
                                case entry of
                                    Group _ ->
                                        el [ Font.size friendScoreSize ] (text <| "(max: " ++ String.fromInt maxScore ++ ")")

                                    Player _ _ ->
                                        none

                                    Friend _ _ extraScore _ ->
                                        if extraScore > 0 then
                                            el [ Font.size friendScoreSize ] (text <| "(" ++ String.fromInt extraScore ++ ")")

                                        else
                                            none
                  }
                , spacerColumn
                ]
            }
        ]


{-| Select colors and initials for each user, spreading out the colors so that users with the same
initial won't get the same color (unless there are a _lot_ of collisions, e.g. more than five Ms.)
-}
assignColors : Colors -> List User -> Dict User Color
assignColors colors users =
    let
        zipRolling xs ys =
            let
                loop zs ts =
                    case ( zs, ts ) of
                        ( [], _ ) ->
                            []

                        ( zh :: zt, th :: tt ) ->
                            ( zh, th ) :: loop zt tt

                        ( zzs, [] ) ->
                            -- start over from the top
                            loop zzs ys
            in
            loop xs ys
    in
    --zipRolling (List.sort users) colors.friends
    users
        |> List.sort
        |> List.indexedMap (\x u -> ( u, colors.friends x ))
        |> Dict.fromList


type alias WordEntry =
    { word : String
    , foundByUser : Bool
    , friendInitials : List ( String, Color )
    }


{-| Show words in two columns, with a summarizing header.
Each word may or may not be shown as found by the user, with zero or more
decorations indicating which other users also found it.
-}
wordList : Colors -> Strings -> WordListSortOrder -> (WordListSortOrder -> msg) -> Int -> List WordEntry -> Bool -> Element msg
wordList colors strings sortOrder resortMsg minimumWordsPerColumn words allKnown =
    let
        -- Sort words according to the preferred ordering. Note: the words are already alpha sorted in
        -- the Dict, and List.sort is stable, so these sorts are on top of that.
        sortWords =
            let
                falseFirst b =
                    if b then
                        0

                    else
                        1
            in
            case sortOrder of
                Found ->
                    -- Tricky: preserve the order of the words the user has found, but reversed
                    -- to put the latest first on screen. Follow them with any unfound words
                    -- (when looking at a previous-day puzzle), sorted alphabetically.
                    List.sortBy
                        (\entry ->
                            ( falseFirst entry.foundByUser
                            , if entry.foundByUser then
                                ""

                              else
                                entry.word
                            )
                        )
                        << List.reverse

                Alpha ->
                    List.sortBy <| \entry -> ( falseFirst entry.foundByUser, falseFirst (isPangram entry.word), entry.word )

                Length ->
                    List.sortBy <| \entry -> ( falseFirst entry.foundByUser, -(String.length entry.word), entry.word )

        wordPairs =
            Array.fromList <|
                sortWords words

        ( col1, col2, col3 ) =
            splitWithMinimum minimumWordsPerColumn wordPairs

        renderColumn pairs =
            column
                [ spacing 2
                , alignTop
                ]
                (List.map (word colors) <| Array.toList pairs)

        -- E.g. "Found 1 word", "Found 0 of 32 words"
        foundMsg =
            let
                found =
                    List.length <|
                        List.filter (\entry -> entry.foundByUser) <|
                            words

                totalMay =
                    if allKnown then
                        Just (Array.length wordPairs)

                    else
                        Nothing
            in
            strings.foundLabel found totalMay
    in
    column
        [ spacing 5
        ]
        [ row
            [ spacing 15 ]
            [ el
                [ width fill
                ]
                (text foundMsg)
            , lightweightButton
                colors
                (strings.sortLabel sortOrder)
                strings.sortDescription
                (Just <| resortMsg <| nextSortOrder sortOrder)
            ]
        , row
            [ Font.light
            , spacing 20
            , paddingXY 10 0
            ]
            [ renderColumn col1
            , renderColumn col2
            , renderColumn col3
            ]
        ]


word : Colors -> WordEntry -> Element msg
word colors entry =
    row [ spacing 5 ]
        [ el
            ((if isPangram entry.word then
                [ Font.medium ]

              else
                []
             )
                ++ (if entry.foundByUser then
                        []

                    else
                        [ Font.color colors.dimForeground, Font.italic ]
                   )
            )
            (text entry.word)
        , row
            [ spacing 2 ]
            (List.map
                (\( str, clr ) ->
                    el
                        [ Font.color clr
                        , Font.size 9
                        , Font.bold
                        ]
                        (text str)
                )
                entry.friendInitials
            )
        ]


{-| Split an array in three, but put values in the second and third parts only after a minumum number
of elements in the first part, and put the extra value in the first half when the total is not a
multiple of three.
-}
splitWithMinimum : Int -> Array a -> ( Array a, Array a, Array a )
splitWithMinimum colMin xs =
    let
        split1 =
            max colMin ((Array.length xs + 2) // 3)

        split2 =
            split1 + max colMin ((Array.length xs - split1 + 1) // 2)
    in
    ( Array.slice 0 split1 xs
    , Array.slice split1 split2 xs
    , Array.slice split2 (Array.length xs) xs
    )


mainThermoStyle : Colors -> ThermoStyle
mainThermoStyle colors =
    { unfilled = colors.secondaryTint
    , filled = colors.primaryTint
    , maxed = colors.queen
    , labelSize = 9
    , bigRadius = 10
    , smallRadius = 4
    , connectorWidth = 5
    , showScore = True
    , showNext = True
    }


smallThermoStyle : Colors -> Color -> ThermoStyle
smallThermoStyle colors foreground =
    let
        base =
            mainThermoStyle colors
    in
    { base
        | filled = foreground
        , bigRadius = 6
        , smallRadius = 2
        , connectorWidth = 2
        , showScore = False
        , showNext = False
    }


{-| Bigger than the thermo label size, smaller than the names.
-}
friendScoreSize : Int
friendScoreSize =
    12


noTouchDelay : Attribute msg
noTouchDelay =
    htmlAttribute <| Html.Attributes.style "touch-action" "manipulation"
