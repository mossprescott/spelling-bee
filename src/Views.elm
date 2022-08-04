module Views exposing
    ( Size
    , WordEntry
    , WordListSortOrder(..)
    , assignColors
    , controlButton
    , entered
    , friendList
    , hintFound
    , hintNone
    , hintWarning
    , hive
    , loadingHeader
    , mainLayout
    , puzzleFooter
    , puzzleHeader
    , scoreBanner
    , wordList
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Puzzle exposing (User, UserInfo, isPangram, wordScore)
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
                    0
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


puzzleHeader : String -> Maybe msg -> Maybe msg -> Element msg
puzzleHeader date previousMsg nextMsg =
    column
        [ centerX
        ]
        [ row
            [ spacing 10
            , Font.size 16
            ]
            [ lightweightButton "←" "Previous Puzzle" previousMsg
            , el [] (text date)
            , lightweightButton "→" "Next Puzzle" nextMsg
            ]
        ]


loadingHeader : Maybe String -> Element msg
loadingHeader msg =
    column
        [ headerFont
        , centerX
        , spacing 5
        ]
        [ el
            [ Font.bold
            , Font.size 24
            ]
            (text "Spelling Bee")
        , el
            [ Font.light
            , Font.size 16
            , Font.italic
            ]
            (text <| Maybe.withDefault "loading…" msg)
        ]


puzzleFooter : String -> Element msg
puzzleFooter editor =
    column
        [ centerX
        , spacing 5
        ]
        [ el [] (text " ") -- space
        , el
            [ Font.light
            , Font.size 16
            ]
            (text <| "Puzzle by " ++ editor)
        , row
            [ Font.light
            , Font.size 16
            ]
            [ text "for the "
            , link
                []
                { url = "https://www.nytimes.com/puzzles/spelling-bee"
                , label = el [ Font.italic, mouseOver [ Font.color blueBgColor ] ] (text "New York Times")
                }
            ]
        , el [] (text " ") -- space
        , row
            [ Font.light
            , Font.size 16
            ]
            [ text "Source and docs "
            , link
                []
                { url = "https://github.com/mossprescott/spelling-bee"
                , label = el [ Font.italic, mouseOver [ Font.color blueBgColor ] ] (text "here")
                }
            ]
        ]


{-| View with the score "thermo" along with the name of the highest level that's been reached.
-}
scoreBanner : Int -> Int -> Bool -> Element msg
scoreBanner maxScore score hasPangram =
    column
        [ spacing 3
        , centerX
        ]
        [ el
            [ Font.size 16
            , centerX
            ]
            (text <| scoreRating maxScore score)
        , scoreThermo mainThermoStyle maxScore score hasPangram
        ]


{-| Text area where letters appear (and can optionally be entered/editted directly.)
-}
entered : (String -> msg) -> msg -> msg -> List Char -> Element msg
entered changedMsg enterMsg shuffleMsg chars =
    row
        [ centerX
        , onKeyStroke <| Dict.fromList [ ( "Enter", enterMsg ), ( " ", shuffleMsg ) ]
        ]
        [ Input.text
            [ Font.center
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


hintWarning : String -> Element msg
hintWarning msg =
    el
        [ centerX
        , height <| px 20
        , Font.size 16
        , Font.light
        , Font.color grayFgColor
        ]
        (text msg)


hintFound : WordEntry -> Element msg
hintFound entry =
    row
        [ centerX
        , height <| px 20
        , Font.size 16
        , Font.light
        , spacing 15
        ]
        [ text <| "+" ++ String.fromInt (wordScore entry.word)
        , word entry
        ]


hive : Char -> Array Char -> Element Char
hive center letters =
    let
        cell letter =
            el
                [ Background.color
                    (if letter == center then
                        blueBgColor

                     else
                        grayBgColor
                    )
                , Font.size 32
                , Border.rounded 5
                , width (px 60)
                , height (px 60)
                , pointer
                , onClick letter
                ]
                (el [ centerX, centerY ]
                    (text (String.fromChar letter))
                )

        letterRow ls =
            row
                [ spacing 5
                , centerX
                , Font.medium
                ]
                (Array.toList <|
                    Array.map cell ls
                )
    in
    column
        [ spacing 5
        , centerX
        , noTouchDelay
        ]
        (List.map letterRow <|
            partition2 2 3 letters
        )


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
controlButton : String -> String -> msg -> Bool -> Element msg
controlButton label description msg enabled =
    buttonImpl label
        description
        (if enabled then
            Just msg

         else
            Nothing
        )
        (Just 60)


{-| HTML button, with similar style as the control buttons, but more compact.
-}
lightweightButton : String -> String -> Maybe msg -> Element msg
lightweightButton label description msg =
    buttonImpl label description msg Nothing


buttonImpl : String -> String -> Maybe msg -> Maybe Int -> Element msg
buttonImpl label description msg minWidth =
    Input.button
        [ Border.rounded 5
        , Border.color blueBgColor
        , Border.width 1
        , Border.solid
        , Font.light
        , Font.color
            (case msg of
                Just _ ->
                    rgb255 0 0 0

                Nothing ->
                    grayBgColor
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


friendList : User -> Dict User UserInfo -> Dict User ( Color, Int ) -> Int -> Int -> Bool -> Element msg
friendList user friends decorations maxScore groupScore groupHasAllPangrams =
    let
        sortedFriends =
            friends
                |> Dict.toList
                |> List.sortBy (\( _, info ) -> -info.score)

        toEntry : User -> FriendEntry
        toEntry u =
            if u == user then
                Player user (userHasPangram user)

            else
                case Dict.get u decorations of
                    Just ( color, extraScore ) ->
                        Friend u color extraScore (userHasPangram u)

                    -- Note: doesn't happen if inputs are correct.
                    Nothing ->
                        Friend u grayBgColor 0 False

        userHasPangram : User -> Bool
        userHasPangram u =
            case Dict.get u friends of
                Just info ->
                    info.hasPangram

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
                [ height (px <| 2 * (smallThermoStyle blueBgColor).bigRadius)
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
        [ text "Friends"
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
                                                "Group"
                                in
                                el (playerFontStyles entry) <| text name
                  }
                , { header = none
                  , width = shrink
                  , view =
                        \( entry, score ) ->
                            case entry of
                                Player _ hasPangram ->
                                    scoreThermo (smallThermoStyle blueBgColor) maxScore score hasPangram

                                Friend _ color _ hasPangram ->
                                    if score > 0 then
                                        scoreThermo (smallThermoStyle color) maxScore score hasPangram

                                    else
                                        none

                                Group hasAllPangrams ->
                                    scoreThermo (smallThermoStyle blueBgColor) maxScore score hasAllPangrams
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
assignColors : List User -> Dict User Color
assignColors users =
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
    Dict.fromList <|
        zipRolling (List.sort users) friendColors


type WordListSortOrder
    = Found
    | Alpha
    | Length


nextSortOrder : WordListSortOrder -> WordListSortOrder
nextSortOrder order =
    case order of
        Found ->
            Alpha

        Alpha ->
            Length

        Length ->
            Found


type alias WordEntry =
    { word : String
    , foundByUser : Bool
    , friendInitials : List ( String, Color )
    }


{-| Show words in two columns, with a summarizing header.
Each word may or may not be shown as found by the user, with zero or more
decorations indicating which other users also found it.
-}
wordList : WordListSortOrder -> (WordListSortOrder -> msg) -> Int -> List WordEntry -> Bool -> Element msg
wordList sortOrder resortMsg minimumWordsPerColumn words allKnown =
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
                    List.reverse

                Alpha ->
                    List.sortBy <| \entry -> ( falseFirst entry.foundByUser, falseFirst (isPangram entry.word) )

                Length ->
                    List.sortBy <| \entry -> ( falseFirst entry.foundByUser, -(String.length entry.word) )

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
                (List.map word <| Array.toList pairs)

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

                numMsg =
                    case ( found, totalMay ) of
                        ( x, Just y ) ->
                            String.fromInt x ++ " of " ++ String.fromInt y

                        ( x, Nothing ) ->
                            String.fromInt x

                wordsStr =
                    case Maybe.withDefault found totalMay of
                        1 ->
                            "word"

                        _ ->
                            "words"
            in
            "Found " ++ numMsg ++ " " ++ wordsStr
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
                -- TODO: chronological
                (case sortOrder of
                    Found ->
                        "f↑"

                    Alpha ->
                        "a↑"

                    Length ->
                        "l↑"
                )
                "Sort Words"
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


word : WordEntry -> Element msg
word entry =
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
                        [ Font.color grayFgColor, Font.italic ]
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


mainThermoStyle : ThermoStyle
mainThermoStyle =
    { unfilled = grayBgColor
    , filled = blueBgColor
    , maxed = yellowColor
    , labelSize = 9
    , bigRadius = 10
    , smallRadius = 4
    , connectorWidth = 5
    , showScore = True
    , showNext = True
    }


smallThermoStyle : Color -> ThermoStyle
smallThermoStyle filledColor =
    { mainThermoStyle
        | filled = filledColor
        , bigRadius = 6
        , smallRadius = 2
        , connectorWidth = 2
        , showScore = False
        , showNext = False
    }


{-| Bigger than th thermo label size, amsller than the names.
-}
friendScoreSize : Int
friendScoreSize =
    12


noTouchDelay : Attribute msg
noTouchDelay =
    htmlAttribute <| Html.Attributes.style "touch-action" "manipulation"
