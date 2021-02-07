module Puzzle exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, dict, int, list, nullable, string)
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


baseUrl : String
baseUrl =
    relativeBaseUrl


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
    }


{-| A place for any data which relates to the group of visible users as a whole.
Called "co-op" in JSON, but how do you spell that in camelCase?
-}
type alias GroupInfo =
    { score : Int
    }


type alias PuzzleId =
    Int


type alias User =
    String


totalScore : Maybe User -> Dict String (List User) -> Int
totalScore userMay found =
    let
        userMatches =
            case userMay of
                Just user ->
                    List.any ((==) user)

                Nothing ->
                    always True

        score word users =
            if userMatches users then
                wordScore word

            else
                0

        go word users acc =
            acc + score word users
    in
    Dict.foldl go 0 found


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


getPuzzle : Maybe PuzzleId -> (Result Http.Error PuzzleResponse -> msg) -> Cmd msg
getPuzzle puzzle_id handler =
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


{-| TODO: the body here will be a PuzzleResponse at some point, mainly so the Guest user can get
information about some demo user (probably Moss).
-}
postWord : (Result Http.Error String -> msg) -> String -> Cmd msg
postWord handler word =
    Http.post
        { url = baseUrl ++ "word"
        , body = Http.jsonBody <| Encode.string word
        , expect = Http.expectWhatever (handler << Result.map (always word))
        }


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
        |> required "editor" string
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


decodeGroupInfo : Decoder GroupInfo
decodeGroupInfo =
    Decode.succeed GroupInfo
        |> required "score" int


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
