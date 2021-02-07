module Views exposing
    ( WordEntry
    , WordListSortOrder(..)
    , assignColors
    , controlButton
    , entered
    , friendList
    , hint
    , hive
    , loadingHeader
    , puzzleHeader
    , scoreBanner
    , threePanel
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
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Puzzle exposing (User, UserInfo, isPangram)
import Views.Constants exposing (..)
import Views.Thermo exposing (..)


{-| Layout the page with three main elements: a header that spans the top of the page,
and two panels which will either sit side-by-side or flow vertically.
-}
threePanel : Element msg -> Element msg -> Element msg -> Element msg
threePanel header content1 content2 =
    column
        [ spacing 15
        , padding 5
        ]
        [ header
        , wrappedRow
            [ spacing 15
            ]
            [ content1
            , content2
            ]
        ]


puzzleHeader : String -> String -> Maybe msg -> Maybe msg -> Element msg
puzzleHeader date editor previousMsg nextMsg =
    column
        [ headerFont
        , spacing 5
        ]
        [ el
            [ Font.bold
            , Font.size 24
            ]
            (text "Spelling Bee")
        , row
            [ spacing 10
            ]
            [ lightweightButton "←" "Previous Puzzle" previousMsg
            , el [] (text date)
            , lightweightButton "→" "Next Puzzle" nextMsg
            ]
        , el
            [ Font.light
            , Font.size 16
            ]
            (text <| "Edited by " ++ editor)
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
        ]


loadingHeader : Element msg
loadingHeader =
    column
        [ headerFont
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
            (text "loading…")
        ]


{-| View with the score "thermo" along with the name of the highest level that's been reached.
-}
scoreBanner : Int -> Int -> Element msg
scoreBanner maxScore score =
    column
        [ spacing 3
        , centerX
        ]
        [ el
            [ Font.size 16
            , centerX
            ]
            (text <| scoreRating maxScore score)
        , scoreThermo mainThermoStyle maxScore score
        ]


{-| Text area where letters appear (and can optionally be entered/editted directly.)
-}
entered : (String -> msg) -> msg -> List Char -> Element msg
entered changedMsg enterMsg word =
    row
        [ centerX
        , onEnter enterMsg
        ]
        [ Input.text
            [ Font.center
            ]
            { text = String.fromList word
            , placeholder = Nothing
            , label = Input.labelHidden "Word"
            , onChange = changedMsg
            }
        ]


onEnter : msg -> Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


hint : Maybe String -> Element msg
hint str =
    el
        [ Font.size 16
        , Font.light
        , Font.color grayFgColor
        , centerX
        , height (px 18)
        ]
        (text <| Maybe.withDefault "" str)


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
    = Player User
    | Friend User String Color -- note: not actually using the initial at this point
    | Group


friendList : User -> Dict User UserInfo -> Dict User ( String, Color ) -> Int -> Int -> Element msg
friendList user friends decorations maxScore groupScore =
    let
        sortedFriends =
            friends
                |> Dict.toList
                |> List.sortBy (\( _, info ) -> -info.score)

        toEntry u =
            if u == user then
                Player user

            else
                case Dict.get u decorations of
                    Just ( initial, color ) ->
                        Friend u initial color

                    Nothing ->
                        Friend u "" grayBgColor

        -- Note: doesn't happen if inputs are correct.
        -- Note: leaving out the Group entry if there are no friends (i.e. Guest mode), but
        -- include it otherwise, even if no other user has any points, because it conveys the max score.
        entries : List ( FriendEntry, Int )
        entries =
            (if List.length sortedFriends > 1 then
                [ ( Group, groupScore ) ]

             else
                []
            )
                ++ List.map (\( u, ui ) -> ( toEntry u, ui.score )) sortedFriends

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
    in
    Element.table
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
                            case entry of
                                Player name ->
                                    el [ Font.bold ] (text name)

                                Friend name _ _ ->
                                    el [] (text name)

                                Group ->
                                    el [ Font.bold, Font.italic ] (text "Group")
              }
            , { header = none
              , width = shrink
              , view =
                    \( entry, score ) ->
                        case entry of
                            Player _ ->
                                scoreThermo (smallThermoStyle blueBgColor) maxScore score

                            Friend _ _ color ->
                                if score > 0 then
                                    scoreThermo (smallThermoStyle color) maxScore score

                                else
                                    none

                            Group ->
                                scoreThermo (smallThermoStyle blueBgColor) maxScore score
              }
            , { header = none
              , width = shrink
              , view =
                    \( entry, _ ) ->
                        centerTextCell <|
                            case entry of
                                Group ->
                                    el [ Font.size (smallThermoStyle blueBgColor).labelSize ] (text <| "(" ++ String.fromInt maxScore ++ ")")

                                _ ->
                                    none
              }
            , spacerColumn
            ]
        }


{-| Select colors and initials for each user, spreading out the colors so that users with the same
initial won't get the same color (unless there are a _lot_ of collisions, e.g. more than five Ms.)
-}
assignColors : List User -> Dict User ( String, Color )
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
                            loop zzs ys

                -- start over from the top
            in
            loop xs ys
    in
    Dict.fromList <|
        List.map (\( u, c ) -> ( u, ( String.slice 0 1 u, c ) )) <|
            zipRolling (List.sort users) friendColors


{-| TODO: `Found` to show the most-recently found word at the top. This will require parsing the words
from the response to a list, so the order is preserved (and fixing the server to return them in
order.)
-}
type WordListSortOrder
    = Alpha
    | Length


nextSortOrder : WordListSortOrder -> WordListSortOrder
nextSortOrder order =
    case order of
        Alpha ->
            Length

        Length ->
            Alpha


type alias WordEntry =
    { foundByUser : Bool
    , friendInitials : List ( String, Color )
    }


{-| Show words in two columns, with a summarizing header.
Each word may or may not be shown as found by the user, with zero or more
decorations indicating which other users also found it.
-}
wordList : WordListSortOrder -> (WordListSortOrder -> msg) -> Int -> Dict String WordEntry -> Bool -> Element msg
wordList sortOrder resortMsg minimumWordsPerColumn words allKnown =
    let
        render ( word, entry ) =
            row [ spacing 5 ]
                [ el
                    ((if isPangram word then
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
                    (text word)
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
                Alpha ->
                    List.sortBy <| \( word, entry ) -> ( falseFirst entry.foundByUser, falseFirst (isPangram word) )

                Length ->
                    List.sortBy <| \( word, entry ) -> ( falseFirst entry.foundByUser, -(String.length word) )

        wordPairs =
            Array.fromList <|
                sortWords <|
                    Dict.toList words

        ( col1, col2, col3 ) =
            splitWithMinimum minimumWordsPerColumn wordPairs

        renderColumn pairs =
            column
                [ spacing 2
                , alignTop
                ]
                (List.map render <| Array.toList pairs)

        -- E.g. "Found 1 word", "Found 0 of 32 words"
        foundMsg =
            let
                found =
                    List.length <|
                        List.filter (\entry -> entry.foundByUser) <|
                            Dict.values <|
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
        , Font.size 16
        ]
        [ row
            [ spacing 15 ]
            [ el
                [ width fill
                ]
                (text foundMsg)
            , lightweightButton
                (case sortOrder of
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
    , showNext = True
    }


smallThermoStyle : Color -> ThermoStyle
smallThermoStyle filledColor =
    { mainThermoStyle
        | filled = filledColor
        , bigRadius = 9
        , smallRadius = 3
        , connectorWidth = 3
        , showNext = False
    }


noTouchDelay : Attribute msg
noTouchDelay =
    htmlAttribute <| Html.Attributes.style "touch-action" "manipulation"
