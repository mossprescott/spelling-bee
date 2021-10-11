module Bee exposing (Model, Msg, beeMain, beeView)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Element
import Element.Font as Font
import Html exposing (Html)
import Http
import List
import Puzzle
    exposing
        ( Puzzle
        , PuzzleBackend
        , PuzzleId
        , PuzzleResponse
        , UserInfo
        , apparentScore
        , unsharedScore
        , wordScore
        )
import Random
import Random.List exposing (shuffle)
import Views
    exposing
        ( WordEntry
        , WordListSortOrder(..)
        , assignColors
        , controlButton
        , entered
        , friendList
        , hint
        , hive
        , loadingHeader
        , puzzleFooter
        , puzzleHeader
        , scoreBanner
        , threePanel
        , wordList
        )
import Views.Constants exposing (..)



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
    , message : Maybe String
    , wordSort : WordListSortOrder
    }


init : PuzzleBackend Msg -> () -> ( Model, Cmd Msg )
init backend flags_unused =
    ( Model Nothing Array.empty [] Nothing Nothing Alpha
    , backend.getPuzzle Nothing ReceivePuzzle
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
    | ReceivePuzzle (Result Http.Error PuzzleResponse)
    | ReceiveWord (Result Http.Error String)


subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.none


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
                    , Cmd.none
                    )

                ReceivePuzzle (Result.Err err) ->
                    ( Debug.log (Debug.toString err)
                        { model
                            | message = Just <| "Error: " ++ Debug.toString err -- TEMP: this is gonna be ugly
                        }
                    , Cmd.none
                    )

                _ ->
                    -- Before the initial load, no other msg is meaningful
                    ( model, Cmd.none )

        Just data ->
            case msg of
                Type c ->
                    ( { model
                        | input = model.input ++ [ c ]
                        , message = Nothing
                      }
                    , Cmd.none
                    )

                -- TODO: validate/filter
                Edit s ->
                    ( { model
                        | input = String.toList (String.toLower s)
                        , message = Nothing
                      }
                    , Cmd.none
                    )

                Delete ->
                    ( { model
                        | input = (String.toList << String.slice 0 -1 << String.fromList) model.input
                        , message = Nothing
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
                            ( model, Cmd.none )

                        Nothing ->
                            ( model, backend.postWord ReceiveWord (String.fromList model.input) )

                ShowPuzzle id ->
                    ( { model
                        | selectedPuzzleId = Just id
                        , message = Nothing
                      }
                    , backend.getPuzzle (Just id) ReceivePuzzle
                    )

                ReceivePuzzle (Result.Ok newData) ->
                    -- TODO: check if a new day's puzzle was loaded and reset the letters
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
                            | message = Just "Error while loading puzzle state"
                        }
                    , Cmd.none
                    )

                ReceiveWord (Result.Ok word) ->
                    let
                        message =
                            "Nice!  +" ++ String.fromInt (wordScore word) ++ " for \"" ++ word ++ "\""
                    in
                    case data.user of
                        Just _ ->
                            -- When authenticated, just reload the puzzle to get the latest state including friends':
                            ( { model
                                | input = []
                                , message = Just message
                              }
                            , backend.getPuzzle model.selectedPuzzleId ReceivePuzzle
                            )

                        Nothing ->
                            -- When not authenticated, just update the state locally:
                            ( { model
                                | data = Maybe.map (tempLocalInsertFound word) model.data
                                , input = []
                                , message = Just message
                              }
                            , Cmd.none
                            )

                ReceiveWord (Result.Err err) ->
                    ( Debug.log (Debug.toString err)
                        { model
                            | input = []
                            , message = Just "Not in word list"
                        }
                    , Cmd.none
                    )


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


tempLocalInsertFound : String -> PuzzleResponse -> PuzzleResponse
tempLocalInsertFound word data =
    { data
        | found =
            Dict.insert
                word
                (data.user |> Maybe.map List.singleton |> Maybe.withDefault [ "Guest" ])
                data.found
    }


beeView : Model -> Html Msg
beeView model =
    let
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
                                data.puzzle.displayDate
                                (Maybe.map ShowPuzzle data.previousPuzzleId)
                                (Maybe.map ShowPuzzle data.nextPuzzleId)

                        ftr =
                            puzzleFooter data.puzzle.editor

                        lp =
                            Element.column
                                [ Element.width (Element.px 320)
                                , Element.alignTop
                                , Element.spacing 10
                                ]
                                [ -- Note: this is the player's score based a local count of the words they found,
                                  -- not the score under .friends (which should be the same), probably because of
                                  -- guest mode?
                                  scoreBanner data.hints.maxScore (apparentScore data.user data)
                                , whenLatest <| entered Edit Submit Shuffle model.input
                                , whenLatest <|
                                    hint <|
                                        case model.message of
                                            Just msg ->
                                                Just msg

                                            Nothing ->
                                                inputError model
                                , hive data.puzzle.centerLetter model.letters
                                    |> Element.map Type
                                , whenLatest <|
                                    Element.row
                                        [ Element.centerX
                                        , Element.spacing 25
                                        , Element.padding 10
                                        ]
                                        [ controlButton "✗" "Delete" Delete (not <| List.isEmpty model.input)
                                        , controlButton "🤷" "Shuffle" Shuffle True
                                        , controlButton "✓" "Submit" Submit (isNothing <| inputError model)
                                        ]
                                ]

                        rp =
                            Element.column
                                [ Element.width (Element.px 300)
                                , Element.alignTop
                                , Element.spacing 15
                                ]
                                [ wordList model.wordSort ResortWords 5 foundMunged (data.puzzle.expiration == Nothing)
                                , friendList user friendsPlaying friendToMeta data.hints.maxScore groupScore
                                ]

                        foundMunged =
                            Dict.map
                                (\_ foundBy ->
                                    WordEntry
                                        (not <| List.isEmpty <| List.filter ((==) user) foundBy)
                                        (List.filterMap
                                            (\u -> Dict.get u colors |> Maybe.map (\c -> ( String.slice 0 1 u, c )))
                                         <|
                                            List.sort foundBy
                                        )
                                )
                                data.found

                        -- Assign colors to just the friends that have logged words today. That
                        -- results in nicer choices of colors sometimes, and potentially
                        -- inconsistent choices from day to day if different people are playing.
                        colors =
                            assignColors <|
                                List.filter ((/=) user) <|
                                    Dict.keys friendsPlaying

                        friendToMeta =
                            Dict.map (\u c -> ( c, unsharedScore u data )) colors

                        friendsPlaying =
                            Dict.filter (\name info -> name == user || info.score > 0) <|
                                friends

                        ( user, friends, groupScore ) =
                            case data.user of
                                Nothing ->
                                    let
                                        localScore =
                                            case data.puzzle.expiration of
                                                Just _ ->
                                                    List.foldl ((+) << wordScore) 0 <| Dict.keys data.found

                                                Nothing ->
                                                    0
                                    in
                                    ( "Guest"
                                    , Dict.insert "Guest" (UserInfo localScore) data.friends
                                    , localScore
                                    )

                                Just name ->
                                    ( name, data.friends, data.group.score )
                    in
                    threePanel hdr lp rp ftr

                Nothing ->
                    threePanel (loadingHeader model.message) Element.none Element.none Element.none
    in
    Element.layout
        [ bodyFont
        , Font.size 20
        ]
        body


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

            else if Dict.member (String.fromList model.input) data.found then
                Just "Already found"

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
