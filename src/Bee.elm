port module Bee exposing
    ( Flags
    , Message(..)
    , Model
    , Msg
    , beeMain
    , beeView
    , startModel
    )

import Animator exposing (Timeline)
import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Element exposing (Color, Element, centerX)
import Element.Background as Background
import Element.Font as Font
import Element.Lazy as Lazy
import Html exposing (Html)
import Http
import Language exposing (Language(..), Strings, stringsFor)
import List
import Puzzle
    exposing
        ( GroupInfo
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
import Set
import Task
import Time
import Views
    exposing
        ( Size
        , WordEntry
        , assignColors
        , colorModeButton
        , controlButton
        , entered
        , friendList
        , hintFound
        , hintNone
        , hintWarning
        , languageButton
        , loadingHeader
        , mainLayout
        , puzzleFooter
        , puzzleHeader
        , scoreBanner
        , wordList
        )
import Views.Constants as Constants
    exposing
        ( ColorMode(..)
        , WordListSortOrder(..)
        , bodyFont
        )
import Views.Hive
    exposing
        ( Position
        , PositionState
        , ShuffleOp
        , applyPositions
        , currentPositions
        , displayHive
        , hive
        , shuffle
        , startPositions
        )
import Views.Permutation exposing (Permutation)



-- TODO: track the displayed date in the location, and initialize it from the URL on load,
-- so you can "deep link" to a particular day.


beeMain : (flags -> Flags) -> PuzzleBackend Msg -> Program flags Model Msg
beeMain decodeFlags backend =
    Browser.element
        { init = init backend << decodeFlags
        , subscriptions = subscriptions
        , update = update backend
        , view = beeView
        }


type alias Flags =
    { dark : Bool
    }


type alias Model =
    { data : Maybe PuzzleResponse

    -- display position for each letter, center first, then outers in the order from the puzzle
    , letters : Timeline PositionState

    -- , used: Array (Timeline Bool)
    , input : List Char
    , selectedPuzzleId : Maybe PuzzleId
    , message : Message
    , wordSort : WordListSortOrder
    , viewport : Size
    , colorMode : ColorMode
    , language : Language
    }


type Message
    = None
    | Warning String -- FIXME: string translated strings here is goofy when the language is changed
    | JustFound String


{-| At the start, we know nothing about any puzzle, and we assume a viewport size corresponding to
a medium-sized phone.
-}
startModel : Flags -> Model
startModel flags =
    Model Nothing
        (Animator.init startPositions)
        []
        Nothing
        None
        Alpha
        { width = 375, height = 675 }
        (if flags.dark then
            Night

         else
            Day
        )
        EN


init : PuzzleBackend Msg -> Flags -> ( Model, Cmd Msg )
init backend flags =
    ( startModel flags
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
    | Submit
    | ResortWords WordListSortOrder
    | SetColorMode ColorMode
    | SetLanguage Language
    | ShowPuzzle PuzzleId
    | DoShuffle ShuffleOp -- Note: broken out as a step so you can see which op was chosen in the debugger
    | Shuffled (Permutation Position)
    | ReceivePuzzle (Result Http.Error PuzzleResponse)
    | ReceiveWord (Result Http.Error String)
    | ReceiveNewViewportSize { width : Int, height : Int }
    | Tick Time.Posix
    | NoOp String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> ReceiveNewViewportSize { width = w, height = h })
        , receiveIsDarkPort
            (\dark ->
                SetColorMode <|
                    if dark then
                        Night

                    else
                        Day
            )
        , animator
            |> Animator.toSubscription Tick model
        ]


animator : Animator.Animator Model
animator =
    Views.Hive.animator .letters (\newLetters model -> { model | letters = newLetters })


update : PuzzleBackend Msg -> Msg -> Model -> ( Model, Cmd Msg )
update backend msg model =
    let
        strings =
            stringsFor model.language
    in
    case model.data of
        Nothing ->
            case msg of
                ReceivePuzzle (Result.Ok data) ->
                    ( { model
                        | data = Just data
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

                -- FIXME: duplicated from below
                ReceiveNewViewportSize size ->
                    ( { model | viewport = size }
                    , Cmd.none
                    )

                -- FIXME: duplicated from below
                SetColorMode mode ->
                    ( { model
                        | colorMode = mode
                      }
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
                    let
                        -- Uniformly 1-to-6, meaning usually 3 or less:
                        numSwaps =
                            Random.int 1 6

                        -- Put the required letter back in the middle (at least) 1/3 of the time:
                        restoreCenter =
                            Random.weighted ( 2, False ) [ ( 1, True ) ]
                    in
                    ( model
                    , Random.generate DoShuffle <|
                        Random.map2 ShuffleOp numSwaps restoreCenter
                    )

                DoShuffle op ->
                    ( model
                    , Random.generate Shuffled (shuffle op (currentPositions model.letters))
                    )

                Shuffled newPositions ->
                    ( { model | letters = applyPositions newPositions model.letters }
                    , Cmd.none
                    )

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

                SetLanguage language ->
                    ( { model
                        | language = language
                      }
                    , Cmd.none
                    )

                ReceivePuzzle (Result.Ok newData) ->
                    let
                        newLetters =
                            if newData.id /= data.id then
                                Animator.init startPositions

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
                                | data = Maybe.map (tempLocalInsertFound strings word) model.data
                                , input = []
                                , message = JustFound word
                              }
                            , Cmd.none
                            )

                ReceiveWord (Result.Err err) ->
                    -- Note: if an error happens here, the word passed local validation,
                    -- so we assume that it's just not part of the solution. However, if
                    -- something goes wrong on the server or network it gets trapped here,
                    -- and a couple of times that's been pretty confusing.
                    ( Debug.log (Debug.toString err)
                        { model
                            | input = []
                            , message = Warning strings.notInWordListMessage
                        }
                    , Cmd.none
                    )

                ReceiveNewViewportSize size ->
                    ( { model | viewport = size }
                    , Cmd.none
                    )

                Tick newTime ->
                    ( Animator.update newTime animator model
                    , Cmd.none
                    )

                NoOp str ->
                    ( Debug.log str model, Cmd.none )


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


tempLocalInsertFound : Strings -> String -> PuzzleResponse -> PuzzleResponse
tempLocalInsertFound strings word data =
    { data
        | found =
            ( word
            , data.user |> Maybe.map List.singleton |> Maybe.withDefault [ strings.guestLabel ]
            )
                :: data.found
    }


beeView : Model -> Html Msg
beeView model =
    let
        colors =
            Constants.themeColors model.colorMode

        strings =
            Language.stringsFor model.language

        decorateHeader hdr =
            Element.row
                [ Element.width Element.fill
                , Element.spacing 10
                ]
                [ hdr
                , colorModeButton colors strings model.colorMode SetColorMode
                , languageButton colors strings model.language SetLanguage
                ]

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
                                strings
                                data.puzzle.displayDate
                                (Maybe.map ShowPuzzle data.previousPuzzleId)
                                (Maybe.map ShowPuzzle data.nextPuzzleId)

                        ftr =
                            puzzleFooter colors strings data.puzzle.editor

                        -- Note: this view is re-constructed every time, because parts of it change
                        -- when a letter is entered or deleted, or for any other user action.
                        gameView =
                            Element.column
                                [ centerX
                                , Element.spacing 10

                                -- , Element.explain Debug.todo
                                ]
                                [ -- Note: this is the player's score based a local count of the words they found,
                                  -- not the score under .friends (which should be the same), probably because of
                                  -- guest mode?
                                  scoreBanner colors strings data.hints.maxScore (apparentScore peopleInfo.user data) (localHasAllPangrams data)
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
                                            decoratedWords data peopleInfo
                                                |> List.filter ((==) word << .word)
                                                |> List.head
                                                |> Maybe.map (hintFound colors)
                                                |> Maybe.withDefault hintNone
                                , case data.puzzle.expiration of
                                    Just _ ->
                                        hive
                                            Type
                                            colors
                                            data.puzzle.centerLetter
                                            (data.puzzle.centerLetter :: data.puzzle.outerLetters)
                                            model.letters
                                            (Set.fromList model.input)

                                    Nothing ->
                                        displayHive
                                            colors
                                            data.puzzle.centerLetter
                                            (data.puzzle.centerLetter :: data.puzzle.outerLetters)
                                            model.letters
                                , whenLatest <|
                                    Element.row
                                        [ Element.centerX
                                        , Element.spacing 25
                                        , Element.padding 10
                                        ]
                                        [ controlButton colors "✗" strings.deleteDescription Delete (not <| List.isEmpty model.input)
                                        , controlButton colors "🤷" strings.shuffleDescription Shuffle True
                                        , controlButton colors "✓" strings.submitDescription Submit (not <| List.isEmpty model.input)
                                        ]
                                ]

                        -- All the info, computed once for non-lazy views:
                        peopleInfo =
                            people model.language model.colorMode data
                    in
                    mainLayout
                        (decorateHeader hdr)
                        gameView
                        (Lazy.lazy4 wordsView model.colorMode model.language model.wordSort data)
                        (Lazy.lazy3 friendsView model.language model.colorMode data)
                        ftr

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
                    mainLayout (loadingHeader strings msg) Element.none Element.none Element.none Element.none
    in
    Element.layout
        [ bodyFont
        , Font.size 16
        , Background.color colors.background
        , Font.color colors.foreground
        ]
        (body desiredColumnWidth model.viewport)



--
-- View construction functions factored out to receive only the specific state that they actually
-- reference, so Element.lazy can be applied to them (and their callers).
--


{-| This view doesn't change when letters are entered or when the letters are shuffled. And it's
probably the most expensive view to construct, so making it lazy should help.
-}
wordsView : ColorMode -> Language -> WordListSortOrder -> PuzzleResponse -> Element Msg
wordsView colorMode language sort data =
    let
        info =
            people language colorMode data
    in
    wordList
        (Constants.themeColors colorMode)
        (Language.stringsFor language)
        sort
        ResortWords
        5
        (decoratedWords data info)
        (data.puzzle.expiration == Nothing)


{-| This view doesn't change when letters are entered or when the letters are shuffled.
-}
friendsView : Language -> ColorMode -> PuzzleResponse -> Element msg
friendsView language colorMode data =
    let
        info =
            people language colorMode data

        friendToMeta =
            Dict.map (\u c -> ( c, unsharedScore u data )) info.friendColors
    in
    friendList
        (Constants.themeColors colorMode)
        (Language.stringsFor language)
        info.user
        info.friendsPlaying
        friendToMeta
        data.hints.maxScore
        info.group.score
        info.group.hasAllPangrams


{-| Aggregation of all the information about people and how to display them.
-}
type alias PeopleInfo =
    { user : String
    , friends : Dict String UserInfo
    , group : GroupInfo
    , friendsPlaying : Dict String UserInfo
    , friendColors : Dict Puzzle.User Color
    }


people : Language -> ColorMode -> PuzzleResponse -> PeopleInfo
people language colorMode data =
    let
        strings =
            Language.stringsFor language

        colors =
            Constants.themeColors colorMode

        -- Assign colors to just the friends that have logged words today. That
        -- results in nicer choices of colors sometimes, and potentially
        -- inconsistent choices from day to day if different people are playing.
        friendColors =
            assignColors colors <|
                List.filter ((/=) user) <|
                    Dict.keys friendsPlaying

        friendsPlaying =
            Dict.filter (\name info -> name == user || info.score > 0) <|
                friends

        ( user, friends, group ) =
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
                    ( strings.guestLabel
                    , Dict.insert strings.guestLabel (UserInfo localScore (localHasPangram data) (localHasAllPangrams data)) data.friends
                    , GroupInfo localScore False
                    )

                Just name ->
                    ( name, data.friends, data.group )
    in
    { user = user
    , friends = friends
    , group = group
    , friendsPlaying = friendsPlaying
    , friendColors = friendColors
    }


decoratedWords : PuzzleResponse -> PeopleInfo -> List WordEntry
decoratedWords data peopleInfo =
    data.found
        |> List.map
            (\( w, foundBy ) ->
                WordEntry
                    w
                    (not <| List.isEmpty <| List.filter ((==) peopleInfo.user) foundBy)
                    (List.filterMap
                        (\u -> Dict.get u peopleInfo.friendColors |> Maybe.map (\c -> ( String.slice 0 1 u, c )))
                     <|
                        List.sort foundBy
                    )
            )


{-| TODO: consider only words for user; false if empty
-}
localHasPangram : PuzzleResponse -> Bool
localHasPangram data =
    data.found
        |> List.any (isPangram << Tuple.first)


localHasAllPangrams : PuzzleResponse -> Bool
localHasAllPangrams data =
    data.found
        |> List.filter (isPangram << Tuple.first)
        |> List.length
        |> (==) data.hints.pangramCount


desiredColumnWidth : Int
desiredColumnWidth =
    300


{-| Some local validation for snappier feedback and less traffic to the backend.
-}
inputError : Model -> Maybe String
inputError model =
    let
        strings =
            stringsFor model.language
    in
    case model.data of
        Nothing ->
            Nothing

        Just data ->
            if List.length model.input == 0 then
                Just ""

            else if List.any ((==) (String.fromList model.input) << Tuple.first) data.found then
                Just strings.alreadyFoundMessage

            else
                let
                    wrong =
                        Set.fromList <|
                            List.filter (\c -> c /= data.puzzle.centerLetter && not (List.member c data.puzzle.outerLetters)) model.input
                in
                if Set.size wrong > 0 then
                    Just <| strings.wrongLettersMessage wrong

                else if List.length model.input < 4 then
                    Just <| strings.tooShortMessage

                else if List.all ((/=) data.puzzle.centerLetter) model.input then
                    Just <| strings.missingCenterLetterMessage

                else
                    Nothing


port receiveIsDarkPort : (Bool -> msg) -> Sub msg
