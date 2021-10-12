module Puzzle exposing
    ( GroupInfo
    , Puzzle
    , PuzzleBackend
    , PuzzleId
    , PuzzleResponse
    , User
    , UserInfo
    , apparentScore
    , herokuBaseUrl
    , isPangram
    , relativeBaseUrl
    , unsharedScore
    , webBackend
    , wordScore
    )

import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, bool, dict, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Set


{-| For debugging: disable Cross-Origin checks in the browser to use this.
-}
herokuBaseUrl : String
herokuBaseUrl =
    "https://spelling-bee-with-enemies.herokuapp.com/"


{-| Actual base path for deployment.
-}
relativeBaseUrl : String
relativeBaseUrl =
    "/"


{-| A puzzle, along with some information about the state of solving it.
-}
type alias PuzzleResponse =
    { user : Maybe User
    , id : PuzzleId
    , nextPuzzleId : Maybe PuzzleId
    , previousPuzzleId : Maybe PuzzleId
    , puzzle : Puzzle
    , found : Dict String (List User)
    , hints : Hints
    , friends : Dict User UserInfo
    , group : GroupInfo
    }


{-| The puzzle to be solved.
-}
type alias Puzzle =
    { expiration : Maybe Int -- TODO: timestamp
    , displayWeekday : String
    , displayDate : String
    , printDate : String
    , editor : String
    , centerLetter : Char
    , outerLetters : List Char
    }


{-| Additional info about the answers, meant to improve the solving experience.
-}
type alias Hints =
    { maxScore : Int
    }


type alias UserInfo =
    { score : Int
    , hasPangram : Bool
    }


{-| A place for any data which relates to the group of visible users as a whole.
Called "co-op" in JSON, but how do you spell that in camelCase?
-}
type alias GroupInfo =
    { score : Int
    , hasAllPangrams : Bool
    }


type alias PuzzleId =
    Int


type alias User =
    String


{-| Score for a user, which may or may not be the player, by adding up the visible words which
they have found. If the given user is not the player, this is the
-}
apparentScore : User -> PuzzleResponse -> Int
apparentScore user resp =
    let
        userMatches userMay =
            case userMay of
                Just u ->
                    List.any ((==) u)

                Nothing ->
                    always True

        score word users =
            -- Basically, the player (the one who's looking) and the other user (the one
            -- they're looking at) *both* have to be in the list of users who found the word
            -- for it to count. This is important when you look at previous days, where words
            -- are present that either or both users may not have found.
            -- If looking at self, then both refer to the same user and it still works.
            -- If I could make this simpler, I certainly would.
            if userMatches (Just user) users && userMatches resp.user users then
                wordScore word

            else
                0

        go word users acc =
            acc + score word users
    in
    Dict.foldl go 0 resp.found


{-| Portion of the total score for a user (not the player), which is accounted for by words that
aren't also found by the player; that is, the score for words that aren't in view when looking at
today's puzzle.
-}
unsharedScore : User -> PuzzleResponse -> Int
unsharedScore user resp =
    let
        sharedScore =
            apparentScore user resp

        totalScore =
            Dict.get user resp.friends
                |> Maybe.map .score
                |> Maybe.withDefault 0
    in
    totalScore - sharedScore


wordScore : String -> Int
wordScore str =
    if String.length str <= 4 then
        1

    else
        String.length str
            + (if isPangram str then
                7

               else
                0
              )


isPangram : String -> Bool
isPangram str =
    (Set.size << Set.fromList << String.toList) str == 7


{-| Wrap up the endpoints in a record, which could be stubbed for testing.
-}
type alias PuzzleBackend msg =
    { getPuzzle : Maybe PuzzleId -> (Result Http.Error PuzzleResponse -> msg) -> Cmd msg
    , postWord : (Result Http.Error String -> msg) -> String -> Cmd msg
    }


webBackend : String -> PuzzleBackend msg
webBackend baseUrl =
    let
        decodePuzzleResponse : Decoder PuzzleResponse
        decodePuzzleResponse =
            Decode.succeed PuzzleResponse
                |> required "user" (nullable string)
                |> required "id" int
                |> required "nextPuzzleId" (nullable int)
                |> required "previousPuzzleId" (nullable int)
                |> required "puzzle" decodePuzzle
                |> required "found" (dict (list string))
                |> required "hints" decodeHints
                |> required "friends" (dict decodeUserInfo)
                |> required "co-op" decodeGroupInfo

        decodePuzzle : Decoder Puzzle
        decodePuzzle =
            Decode.succeed Puzzle
                |> required "expiration" (nullable int)
                -- TODO: timestamp
                |> required "displayWeekday" string
                |> required "displayDate" string
                |> required "printDate" string
                |> optional "editor" string "Anonymous"
                |> required "centerLetter" char
                |> required "outerLetters" (list char)

        decodeHints : Decoder Hints
        decodeHints =
            Decode.succeed Hints
                |> required "maxScore" int

        decodeUserInfo : Decoder UserInfo
        decodeUserInfo =
            Decode.succeed UserInfo
                |> required "score" int
                |> required "hasPangram" bool

        decodeGroupInfo : Decoder GroupInfo
        decodeGroupInfo =
            -- TODO: require hasAllPangrams
            Decode.succeed GroupInfo
                |> required "score" int
                |> required "hasAllPangrams" bool

        char : Decoder Char
        char =
            let
                toChar str =
                    case String.toList str of
                        [ c ] ->
                            Decode.succeed c

                        _ ->
                            Decode.fail "expected single char"
            in
            string |> andThen toChar
    in
    { getPuzzle =
        \puzzle_id handler ->
            case puzzle_id of
                Nothing ->
                    Http.get
                        { url = baseUrl ++ "puzzle"
                        , expect = Http.expectJson handler decodePuzzleResponse
                        }

                Just pid ->
                    Http.get
                        { url = baseUrl ++ "puzzle/" ++ String.fromInt pid
                        , expect = Http.expectJson handler decodePuzzleResponse
                        }

    {- TODO: the body here will be a PuzzleResponse at some point, mainly so the Guest user can get
       information about some demo user (probably Moss).
    -}
    , postWord =
        \handler word ->
            Http.post
                { url = baseUrl ++ "word"
                , body = Http.jsonBody <| Encode.string word
                , expect = Http.expectWhatever (handler << Result.map (always word))
                }
    }
