module Language exposing
    ( Language(..)
    , Strings
    , rotate
    , stringsFor
    )

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
        \level ->
            case level of
                ScoreLevel0 ->
                    "Beginner"

                ScoreLevel1 ->
                    "Good Start"

                ScoreLevel2 ->
                    "Moving Up"

                ScoreLevel3 ->
                    "Good"

                ScoreLevel4 ->
                    "Solid"

                ScoreLevel5 ->
                    "Nice"

                ScoreLevel6 ->
                    "Great"

                ScoreLevel7 ->
                    "Amazing"

                ScoreLevel8 ->
                    "Genius"

                ScoreLevel9 ->
                    "Queen Bee"
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
        \level ->
            case level of
                ScoreLevel0 ->
                    "Principiante"

                ScoreLevel1 ->
                    "Buen Comienzo"

                ScoreLevel2 ->
                    "Mejorando"

                ScoreLevel3 ->
                    "Bien"

                ScoreLevel4 ->
                    "Fuerte"

                ScoreLevel5 ->
                    "Lindo"

                ScoreLevel6 ->
                    "Excelente"

                ScoreLevel7 ->
                    "Increíble"

                ScoreLevel8 ->
                    "Genio"

                ScoreLevel9 ->
                    "Abeja Reina"
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
