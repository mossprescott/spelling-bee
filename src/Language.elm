module Language exposing
    ( Language(..)
    , Strings
    , rotate
    , stringsFor
    )

import Set exposing (Set)
import Views.Constants exposing (ScoreLevel(..), WordListSortOrder(..))


type Language
    = EN
    | DE
    | ES


rotate : Language -> Language
rotate language =
    case language of
        EN ->
            DE

        DE ->
            ES

        ES ->
            EN


{-| A record containing all explicit Strings which the user can encounter while playing the game,
with the following exceptions:

  - symbols and emoji, which are "universal" by design
  - error messages that arise from programming errors or unexpected backend problems
  - strings that are embedded in the puzzle from the source (e.g. the puzzle's date)

-}
type alias Strings =
    { -- Header stuff:
      icon : String
    , titleLabel : String
    , loadingLabel : String
    , editorLabel : String -> String
    , attributionLabel : String
    , nytLabel : String
    , sourceLabel : String
    , hereLabel : String

    -- Puzzle controls:
    , scoreLabel : ScoreLevel -> String
    , foundLabel : Int -> Maybe Int -> String
    , sortLabel : WordListSortOrder -> String
    , friendsLabel : String
    , groupLabel : String
    , guestLabel : String

    -- Error messages:
    , alreadyFoundMessage : String
    , wrongLettersMessage : Set Char -> String
    , tooShortMessage : String
    , missingCenterLetterMessage : String
    , notInWordListMessage : String

    -- Accessibility labels:
    , previousPuzzleDescription : String
    , nextPuzzleDescription : String
    , colorModeDescription : String
    , languageDescription : String
    , sortDescription : String
    , deleteDescription : String
    , shuffleDescription : String
    , submitDescription : String
    }


enStrings : Strings
enStrings =
    { icon = "🇺🇸"
    , titleLabel = "Spelling Bee"
    , loadingLabel = "loading…"
    , editorLabel = \ed -> "Puzzle by " ++ ed
    , attributionLabel = "for the "
    , nytLabel = "New York Times"
    , sourceLabel = "Source and docs "
    , hereLabel = "here"

    -- Puzzle controls:
    , scoreLabel =
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
    , friendsLabel = "Friends"
    , groupLabel = "Group"
    , guestLabel = "Guest"

    -- Error messages:
    , alreadyFoundMessage = "Already Found"
    , wrongLettersMessage =
        wrongLettersMessage
            (\ls -> "Wrong letter: " ++ ls)
            (\ls -> "Wrong letters: " ++ ls)
    , tooShortMessage = "Too short"
    , missingCenterLetterMessage = "Missing center letter"
    , notInWordListMessage = "Not in word list"

    -- Accessibility:
    , previousPuzzleDescription = "Previous Puzzle"
    , nextPuzzleDescription = "Next Puzzle"
    , colorModeDescription = "Color Mode"
    , languageDescription = "Language"
    , sortDescription = "Sort Order"
    , deleteDescription = "Delete"
    , shuffleDescription = "Shuffle"
    , submitDescription = "Submit"
    }


deStrings : Strings
deStrings =
    { icon = "🇩🇪"
    , titleLabel = "Spelling Bee"
    , loadingLabel = "lädt…"
    , editorLabel = \ed -> "Puzzle von " ++ ed
    , attributionLabel = "für die "
    , nytLabel = "New York Times"
    , sourceLabel = "Quelle und Dokumente "
    , hereLabel = "hier"

    -- Puzzle controls:
    , scoreLabel =
        scoreLabels
            "Anfänger"
            [ "Guter Anfang"
            , "Aufsteigend"
            , "Gut"
            , "Solid"
            , "Nett"
            , "Großartig"
            , "Erstaunlich"
            , "Genie"
            , "Bienenkönigin"
            ]
    , foundLabel =
        foundLabels
            "1 Word gefunden"
            (\m -> m ++ " Wörter gefunden")
            (\m n -> m ++ " von " ++ n ++ " Wörter gefunden")
    , sortLabel =
        \order ->
            case order of
                Found ->
                    "g↑"

                Alpha ->
                    "a↑"

                Length ->
                    "l↑"
    , friendsLabel = "Freunde"
    , groupLabel = "Gruppe"
    , guestLabel = "Gast"

    -- Error messages:
    , alreadyFoundMessage = "schon gefunden"
    , wrongLettersMessage =
        wrongLettersMessage
            (\ls -> "falscher Buchstabe: " ++ ls)
            (\ls -> "falsche Buchstaben: " ++ ls)
    , tooShortMessage = "zu kurz"
    , missingCenterLetterMessage = "fehlender Mittelbuchstabe "
    , notInWordListMessage = "nicht in der Wortliste"

    -- Accessibility:
    , previousPuzzleDescription = "<<Previous Puzzle>>"
    , nextPuzzleDescription = "Nächste Puzzle"
    , colorModeDescription = "Tag un Nacht"
    , languageDescription = "Sprache"
    , sortDescription = "<<Sort Order>>"
    , deleteDescription = "<<Delete>>"
    , shuffleDescription = "<<Shuffle>>"
    , submitDescription = "<<Submit>>"
    }


esStrings : Strings
esStrings =
    { icon = "🇲🇽"
    , titleLabel = "<<Spelling Bee>>"
    , loadingLabel = "<<loading…>>"
    , editorLabel = \ed -> "<<Puzzle by>> " ++ ed
    , attributionLabel = "<<for the>> "
    , nytLabel = "New York Times"
    , sourceLabel = "<<Source and docs>> "
    , hereLabel = "aqui"

    -- Puzzle controls:
    , scoreLabel =
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
    , friendsLabel = "Amigos"
    , groupLabel = "<<Group>>"
    , guestLabel = "Visitante"

    -- Error messages:
    , alreadyFoundMessage = "<<Already Found>>"
    , wrongLettersMessage =
        wrongLettersMessage
            (\ls -> "<<Wrong letter>>: " ++ ls)
            (\ls -> "<<Wrong letters>>: " ++ ls)
    , tooShortMessage = "<<Too short>>"
    , missingCenterLetterMessage = "<<Missing center letter>>"
    , notInWordListMessage = "<<Not in word list>>"

    -- Accessibility:
    , previousPuzzleDescription = "<<Previous Puzzle>>"
    , nextPuzzleDescription = "<<Next Puzzle>>"
    , colorModeDescription = "<<Color Mode>>"
    , languageDescription = "<<Language>>"
    , sortDescription = "Orden de Clasificación"
    , deleteDescription = "<<Delete>>"
    , shuffleDescription = "<<Shuffle>>"
    , submitDescription = "<<Submit>>"
    }


stringsFor : Language -> Strings
stringsFor language =
    case language of
        EN ->
            enStrings

        DE ->
            deStrings

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


wrongLettersMessage :
    (String -> String)
    -> (String -> String)
    -> (Set Char -> String)
wrongLettersMessage oneLetter letters wrong =
    let
        wrongStr =
            wrong
                |> Set.toList
                |> List.intersperse ' '
                |> String.fromList
    in
    if Set.size wrong == 1 then
        oneLetter wrongStr

    else
        letters wrongStr
