module Bee exposing
    ( Message(..)
    , Model
    , Msg
    , beeMain
    , beeView
    , startModel
    )

import Array exposing (Array)
import Browser
import Browser.Dom
import Browser.Events
import Dict
import Element exposing (centerX)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Http
import List
import Puzzle
    exposing
        ( GroupInfo
        , Puzzle
        , PuzzleBackend
        , PuzzleId
        , PuzzleResponse
        , UserInfo
        , apparentScore
        , isPangram
        , unsharedScore
        , wordScore
        )
import Random
import Random.List exposing (shuffle)
import Set
import Task
import Views
    exposing
        ( Size
        , WordEntry
        , WordListSortOrder(..)
        , assignColors
        , colorModeButton
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
import Views.Constants as Constants exposing (ColorMode(..), bodyFont)



-- TODO: track the displayed date in the location, and initialize it from the URL on load,
-- so you can "deep link" to a particular day.


beeMain : PuzzleBackend Msg -> Program () Model Msg
beeMain backend =
    Browser.element
        { init = init backend
        , subscriptions = subscriptions
        , update = update backend
        , view = beeView
        }


type alias Model =
    { data : Maybe PuzzleResponse
    , letters : Array Char -- the letters of the puzzle, in an arbitrary order for display
    , input : List Char
    , selectedPuzzleId : Maybe PuzzleId
    , message : Message
    , wordSort : WordListSortOrder
    , viewport : Size
    , colorMode : ColorMode
    }


type Message
    = None
    | Warning String
    | JustFound String


{-| At the start, we know nothing about any puzzle, and we assume a viewport size corresponding to
a medium-sized phone.
-}
startModel : Model
startModel =
    Model Nothing Array.empty [] Nothing None Alpha { width = 375, height = 675 } Day


init : PuzzleBackend Msg -> () -> ( Model, Cmd Msg )
init backend flags_unused =
    ( startModel
    , Cmd.batch
        [ Task.perform (\vp -> ReceiveNewViewportSize { width = round vp.viewport.width, height = round vp.viewport.height }) Browser.Dom.getViewport
        , backend.getPuzzle Nothing ReceivePuzzle
        ]
    )


type Msg
    = Type Char
    | Edit String
    | Delete
    | Shuffle
    | Shuffled (Array Char)
    | ResortWords WordListSortOrder
    | Submit
    | ShowPuzzle PuzzleId
    | SetColorMode ColorMode
    | ReceivePuzzle (Result Http.Error PuzzleResponse)
    | ReceiveWord (Result Http.Error String)
    | ReceiveNewViewportSize { width : Int, height : Int }
    | NoOp String


subscriptions : model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> ReceiveNewViewportSize { width = w, height = h })


update : PuzzleBackend Msg -> Msg -> Model -> ( Model, Cmd Msg )
update backend msg model =
    case model.data of
        Nothing ->
            case msg of
                ReceivePuzzle (Result.Ok data) ->
                    ( { model
                        | data = Just data
                        , letters = startLetters data.puzzle
                      }
                    , initialFocusTask model
                    )

                ReceivePuzzle (Result.Err err) ->
                    ( Debug.log (Debug.toString err)
                        { model
                            | message = Warning <| "Error: " ++ Debug.toString err -- TEMP: this is gonna be ugly
                        }
                    , Cmd.none
                    )

                ReceiveNewViewportSize size ->
                    ( { model | viewport = size }
                    , Cmd.none
                    )

                _ ->
                    -- Before the initial load, no other msg is meaningful
                    ( Debug.log ("Puzzle not loaded; ignoring msg" ++ Debug.toString msg)
                        model
                    , Cmd.none
                    )

        Just data ->
            case msg of
                Type c ->
                    ( { model
                        | input = model.input ++ [ c ]
                        , message = None
                      }
                    , Cmd.none
                    )

                -- TODO: validate/filter
                Edit s ->
                    ( { model
                        | input = String.toList (String.toLower s)
                        , message = None
                      }
                    , Cmd.none
                    )

                Delete ->
                    ( { model
                        | input = (String.toList << String.slice 0 -1 << String.fromList) model.input
                        , message = None
                      }
                    , Cmd.none
                    )

                Shuffle ->
                    ( model, Random.generate (Array.fromList >> Shuffled) ((Array.toList >> shuffle) model.letters) )

                Shuffled letters ->
                    ( { model | letters = letters }, Cmd.none )

                ResortWords order ->
                    ( { model | wordSort = order }
                    , Cmd.none
                    )

                Submit ->
                    case inputError model of
                        Just _ ->
                            ( { model | input = [] }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, backend.postWord ReceiveWord (String.fromList model.input) )

                ShowPuzzle id ->
                    ( { model
                        | selectedPuzzleId = Just id
                        , message = None
                      }
                    , backend.getPuzzle (Just id) ReceivePuzzle
                    )

                SetColorMode mode ->
                    ( { model
                        | colorMode = mode
                      }
                    , Cmd.none
                    )

                ReceivePuzzle (Result.Ok newData) ->
                    let
                        newLetters =
                            if newData.id /= data.id then
                                startLetters newData.puzzle

                            else
                                model.letters
                    in
                    ( { model
                        | data = Just newData
                        , letters = newLetters
                      }
                    , Cmd.none
                    )

                ReceivePuzzle (Result.Err err) ->
                    ( Debug.log (Debug.toString err)
                        { model
                            | message = Warning "Error while loading puzzle state"
                        }
                    , Cmd.none
                    )

                ReceiveWord (Result.Ok word) ->
                    case data.user of
                        Just _ ->
                            -- When authenticated, just reload the puzzle to get the latest state including friends':
                            ( { model
                                | input = []
                                , message = JustFound word
                              }
                            , backend.getPuzzle model.selectedPuzzleId ReceivePuzzle
                            )

                        Nothing ->
                            -- When not authenticated, hackishly update the state locally:
                            ( { model
                                | data = Maybe.map (tempLocalInsertFound word) model.data
                                , input = []
                                , message = JustFound word
                              }
                            , Cmd.none
                            )

                ReceiveWord (Result.Err err) ->
                    ( Debug.log (Debug.toString err)
                        { model
                            | input = []
                            , message = Warning "Not in word list"
                        }
                    , Cmd.none
                    )

                ReceiveNewViewportSize size ->
                    ( { model | viewport = size }
                    , Cmd.none
                    )

                NoOp str ->
                    ( Debug.log str model, Cmd.none )


startLetters : Puzzle -> Array Char
startLetters puzzle =
    let
        outer =
            Array.fromList puzzle.outerLetters
    in
    Array.append
        (Array.slice 0 3 outer)
        (Array.append
            (Array.fromList [ puzzle.centerLetter ])
            (Array.slice 3 6 outer)
        )


{-| For the benefit of desktop users, start with the input field focused, based on there
being a lot of extra space. Note: this could annoy tablet users, who might rather tap on the
buttons than use an on-screen keyboard, even if there's a lot of space.
-}
initialFocusTask : Model -> Cmd Msg
initialFocusTask model =
    let
        probablyDesktop =
            model.viewport.width > 2 * desiredColumnWidth && model.viewport.height > 2 * desiredColumnWidth
    in
    if probablyDesktop then
        Task.attempt (\err -> NoOp (Debug.toString err)) (Browser.Dom.focus "input")

    else
        Cmd.none


tempLocalInsertFound : String -> PuzzleResponse -> PuzzleResponse
tempLocalInsertFound word data =
    { data
        | found =
            ( word
            , data.user |> Maybe.map List.singleton |> Maybe.withDefault [ "Guest" ]
            )
                :: data.found
    }


beeView : Model -> Html Msg
beeView model =
    let
        colors =
            Constants.themeColors model.colorMode

        body =
            case model.data of
                Just data ->
                    let
                        whenLatest el =
                            case data.puzzle.expiration of
                                Just _ ->
                                    el

                                Nothing ->
                                    Element.none

                        hdr =
                            puzzleHeader
                                colors
                                data.puzzle.displayDate
                                (Maybe.map ShowPuzzle data.previousPuzzleId)
                                (Maybe.map ShowPuzzle data.nextPuzzleId)

                        ftr =
                            puzzleFooter colors data.puzzle.editor

                        gameView =
                            Element.column
                                [ centerX
                                , Element.spacing 10
                                ]
                                [ -- Note: this is the player's score based a local count of the words they found,
                                  -- not the score under .friends (which should be the same), probably because of
                                  -- guest mode?
                                  scoreBanner colors data.hints.maxScore (apparentScore user data) localHasPangram
                                , whenLatest <| entered colors Edit Submit Shuffle model.input
                                , whenLatest <|
                                    case model.message of
                                        None ->
                                            case inputError model of
                                                Just str ->
                                                    hintWarning colors str

                                                Nothing ->
                                                    hintNone

                                        Warning msg ->
                                            hintWarning colors msg

                                        JustFound word ->
                                            foundMunged
                                                |> List.filter ((==) word << .word)
                                                |> List.head
                                                |> Maybe.map (hintFound colors)
                                                |> Maybe.withDefault hintNone
                                , hive colors data.puzzle.centerLetter model.letters
                                    |> Element.map Type
                                , whenLatest <|
                                    Element.row
                                        [ Element.centerX
                                        , Element.spacing 25
                                        , Element.padding 10
                                        ]
                                        [ controlButton colors "✗" "Delete" Delete (not <| List.isEmpty model.input)
                                        , controlButton colors "🤷" "Shuffle" Shuffle True
                                        , controlButton colors "✓" "Submit" Submit (not <| List.isEmpty model.input)
                                        ]
                                ]

                        wordsView =
                            wordList colors model.wordSort ResortWords 5 foundMunged (data.puzzle.expiration == Nothing)

                        friendsView =
                            friendList colors user friendsPlaying friendToMeta data.hints.maxScore groupInfo.score groupInfo.hasAllPangrams

                        foundMunged =
                            List.map
                                (\( w, foundBy ) ->
                                    WordEntry
                                        w
                                        (not <| List.isEmpty <| List.filter ((==) user) foundBy)
                                        (List.filterMap
                                            (\u -> Dict.get u friendColors |> Maybe.map (\c -> ( String.slice 0 1 u, c )))
                                         <|
                                            List.sort foundBy
                                        )
                                )
                                data.found

                        -- Assign colors to just the friends that have logged words today. That
                        -- results in nicer choices of colors sometimes, and potentially
                        -- inconsistent choices from day to day if different people are playing.
                        friendColors =
                            assignColors colors <|
                                List.filter ((/=) user) <|
                                    Dict.keys friendsPlaying

                        friendToMeta =
                            Dict.map (\u c -> ( c, unsharedScore u data )) friendColors

                        friendsPlaying =
                            Dict.filter (\name info -> name == user || info.score > 0) <|
                                friends

                        -- TODO: consider only words for user; false if empty
                        localHasPangram =
                            data.found
                                |> List.any (isPangram << Tuple.first)

                        ( user, friends, groupInfo ) =
                            case data.user of
                                Nothing ->
                                    let
                                        localScore =
                                            case data.puzzle.expiration of
                                                Just _ ->
                                                    data.found
                                                        |> List.foldl ((+) << wordScore << Tuple.first) 0

                                                Nothing ->
                                                    0
                                    in
                                    ( "Guest"
                                    , Dict.insert "Guest" (UserInfo localScore localHasPangram) data.friends
                                    , GroupInfo localScore False
                                    )

                                Just name ->
                                    ( name, data.friends, data.group )
                    in
                    mainLayout hdr gameView wordsView friendsView ftr

                Nothing ->
                    let
                        msg =
                            case model.message of
                                None ->
                                    Nothing

                                Warning str ->
                                    Just str

                                JustFound _ ->
                                    Nothing
                    in
                    mainLayout (loadingHeader msg) Element.none Element.none Element.none Element.none
    in
    Element.layout
        [ bodyFont
        , Font.size 16
        , Background.color colors.background
        , Font.color colors.foreground
        ]
        (body desiredColumnWidth model.viewport)


desiredColumnWidth =
    300


{-| Some local validation for snappier feedback and less traffic to the backend.
-}
inputError : Model -> Maybe String
inputError model =
    case model.data of
        Nothing ->
            Nothing

        Just data ->
            if List.length model.input == 0 then
                Just ""

            else if List.any ((==) (String.fromList model.input) << Tuple.first) data.found then
                Just "Already found"

            else
                let
                    wrong =
                        Set.fromList <|
                            List.filter (\c -> c /= data.puzzle.centerLetter && not (List.member c data.puzzle.outerLetters)) model.input
                in
                if Set.size wrong > 0 then
                    let
                        pluralized =
                            if Set.size wrong == 1 then
                                "Wrong letter"

                            else
                                "Wrong letters"

                        wrongStr =
                            wrong
                                |> Set.toList
                                |> List.intersperse ' '
                                |> String.fromList
                    in
                    Just <| pluralized ++ ": " ++ wrongStr

                else if List.length model.input < 4 then
                    Just "Too short"

                else if List.all ((/=) data.puzzle.centerLetter) model.input then
                    Just "Missing center letter"

                else
                    Nothing


isNothing : Maybe a -> Bool
isNothing ma =
    case ma of
        Just _ ->
            False

        Nothing ->
            True
