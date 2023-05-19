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
        foundLabels
            "Found 1 word"
            (\m -> "Found " ++ m ++ " words")
            (\m n -> "Found " ++ m ++ " of " ++ n ++ " words")
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
        foundLabels
            "Encontraste 1 palabra"
            (\m -> "Encontraste " ++ m ++ " palabras")
            (\m n -> "Encontraste " ++ m ++ " de " ++ n ++ " palabras")
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


{-| Cases:

  - Found 1 word
  - Found n words
  - Found n of m words

-}
foundLabels :
    String
    -> (String -> String)
    -> (String -> String -> String)
    -> (Int -> Maybe Int -> String)
foundLabels oneWord words ofTotal =
    \found totalMay ->
        case ( found, totalMay ) of
            ( 1, Nothing ) ->
                oneWord

            ( _, Nothing ) ->
                words (String.fromInt found)

            ( _, Just total ) ->
                ofTotal (String.fromInt found) (String.fromInt total)
