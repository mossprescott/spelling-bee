module Language exposing
    ( Language(..)
    , Strings
    , rotate
    , stringsFor
    )

import Array
import Views.Constants exposing (ScoreLevel(..), WordListSortOrder(..))


type Language
    = EN
    | ES


rotate : Language -> Language
rotate language =
    case language of
        EN ->
            ES

        ES ->
            EN


type alias Strings =
    { scoreLabel : ScoreLevel -> String
    , foundLabel : Int -> Maybe Int -> String
    , sortLabel : WordListSortOrder -> String
    , sortDescription : String
    , friendsLabel : String
    , guestLabel : String
    }


enStrings : Strings
enStrings =
    { scoreLabel =
        scoreLabels
            "Beginner"
            [ "Good Start"
            , "Moving Up"
            , "Good"
            , "Solid"
            , "Nice"
            , "Great"
            , "Amazing"
            , "Genius"
            , "Queen Bee"
            ]
    , foundLabel =
        \found totalMay ->
            let
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
    , sortLabel =
        \order ->
            case order of
                Found ->
                    "f↑"

                Alpha ->
                    "a↑"

                Length ->
                    "l↑"
    , sortDescription = "Sort Order"
    , friendsLabel = "Friends"
    , guestLabel = "Guest"
    }


esStrings : Strings
esStrings =
    { scoreLabel =
        scoreLabels
            "Principiante"
            [ "Buen Comienzo"
            , "Mejorando"
            , "Bien"
            , "Fuerte"
            , "Lindo"
            , "Excelente"
            , "Increíble"
            , "Genio"
            , "Abeja Reina"
            ]
    , foundLabel =
        \found totalMay ->
            let
                numMsg =
                    case ( found, totalMay ) of
                        ( x, Just y ) ->
                            String.fromInt x ++ " de " ++ String.fromInt y

                        ( x, Nothing ) ->
                            String.fromInt x

                wordsStr =
                    case Maybe.withDefault found totalMay of
                        1 ->
                            "palabra"

                        _ ->
                            "palabras"
            in
            "Encontraste " ++ numMsg ++ " " ++ wordsStr
    , sortLabel =
        \order ->
            case order of
                Found ->
                    "e↑"

                Alpha ->
                    "a↑"

                Length ->
                    "l↑"
    , sortDescription = "Orden de Clasificación"
    , friendsLabel = "Amigos"
    , guestLabel = "Visitante"
    }


stringsFor : Language -> Strings
stringsFor language =
    case language of
        EN ->
            enStrings

        ES ->
            esStrings



-- Helpers:


scoreLabels : String -> List String -> ScoreLevel -> String
scoreLabels beginnerStr strs =
    let
        pairs =
            List.map2 Tuple.pair strs [ ScoreLevel1, ScoreLevel2, ScoreLevel3, ScoreLevel4, ScoreLevel5, ScoreLevel6, ScoreLevel7, ScoreLevel8, ScoreLevel9 ]
    in
    \level ->
        pairs
            |> List.filterMap
                (\( s, l ) ->
                    if l == level then
                        Just s

                    else
                        Nothing
                )
            |> List.head
            |> Maybe.withDefault beginnerStr
