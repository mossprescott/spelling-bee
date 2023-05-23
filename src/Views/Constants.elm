module Views.Constants exposing
    ( ColorMode(..)
    , Colors
    , ScoreLevel(..)
    , WordListSortOrder(..)
    , bodyFont
    , headerFont
    , nextSortOrder
    , rotate
    , themeColors
    )

import Array
import Element exposing (Attribute, Color, rgb255)
import Element.Font as Font
import Tuple exposing (first)


{-| "Theme"?
-}
type alias Colors =
    { background : Color
    , foreground : Color

    -- For the center letter button and the player's own thermo:
    , primaryTint : Color

    -- For the rest of the letters and other low-contrast fill:
    , secondaryTint : Color

    -- For words you didn't find:
    , dimForeground : Color

    -- For the text of controls that aren't currently active:
    , inactiveForeground : Color

    -- For controls that are active (e.g. buttons and links):
    , activeHilite : Color

    -- A special color for "Queen Bee" status:
    , queen : Color

    -- Enough alternative colors to assign one to each friend such that: most of the time, each
    -- friend gets a unique color; when you have a lot of friends, no two friends have the same
    -- color _and_ the same initial.
    , friends : Int -> Color
    }


type ColorMode
    = Day
    | Night


rotate : ColorMode -> ColorMode
rotate mode =
    case mode of
        Day ->
            Night

        Night ->
            Day


{-| Index into a non-empty List, repeated indefinitely.
-}
getRolling : a -> List a -> Int -> a
getRolling first rest index =
    -- This seems a little awkward, but at least it's hidden in this function.
    -- When this is partially applied, there should only be one Array instance
    -- created and re-used indefinitely.
    let
        arr =
            Array.fromList <| first :: rest
    in
    Array.get (modBy (Array.length arr) index) arr |> Maybe.withDefault first


dayColors : Colors
dayColors =
    { background = rgb255 255 255 255
    , foreground = rgb255 0 0 0

    -- Light blue
    , primaryTint = rgb255 63 191 255

    -- Light grey
    , secondaryTint = rgb255 223 223 223

    -- A darker gray for text:
    , dimForeground = rgb255 127 127 127

    -- Same as the regular background grey:
    , inactiveForeground = rgb255 223 223 223

    -- Same as the primary tint color:
    , activeHilite = rgb255 63 191 255

    -- Yellow
    , queen = rgb255 255 255 63
    , friends =
        getRolling
            -- purple:
            (rgb255 191 63 255)
            [ -- red:
              rgb255 255 63 63

            -- orange:
            , rgb255 255 159 63

            -- green:
            , rgb255 143 191 47
            ]
    }


nightColors : Colors
nightColors =
    { background = rgb255 0 0 0
    , foreground = rgb255 223 223 223

    -- Light blue (75%)
    , primaryTint = rgb255 47 143 191

    -- Light grey
    , secondaryTint = rgb255 63 63 63

    -- A little brighter for text:
    , dimForeground = rgb255 175 175 175

    -- Same as the regular background grey:
    , inactiveForeground = rgb255 127 127 127

    -- Same as the primary tint color:
    , activeHilite = rgb255 47 143 191

    -- A sort of dimmer honey:
    , queen = rgb255 229 191 0

    -- Same as day, reduced to 85% value:
    , friends =
        getRolling
            -- purple:
            (rgb255 162 54 216)
            [ -- red:
              rgb255 216 54 54

            -- orange (95%):
            , rgb255 242 151 60

            -- green (60%, down from 75%):
            , rgb255 114 153 38
            ]
    }


themeColors : ColorMode -> Colors
themeColors mode =
    case mode of
        Day ->
            dayColors

        Night ->
            nightColors


{-| Note: in theory, this is appropriate for something associated with the NY Times, but it's not
attractive, especially when juxtaposed with Source Sans below.
-}
headerFont : Attribute msg
headerFont =
    Font.family
        [ Font.typeface "Times New Roman"
        , Font.typeface "Times"
        , Font.serif
        ]


bodyFont : Attribute msg
bodyFont =
    Font.family
        [ Font.typeface "Source Sans Pro"
        , Font.typeface "Helvetica Neue"
        , Font.sansSerif
        ]


type WordListSortOrder
    = Found
    | Alpha
    | Length


nextSortOrder : WordListSortOrder -> WordListSortOrder
nextSortOrder order =
    case order of
        Found ->
            Alpha

        Alpha ->
            Length

        Length ->
            Found


type ScoreLevel
    = ScoreLevel0
    | ScoreLevel1
    | ScoreLevel2
    | ScoreLevel3
    | ScoreLevel4
    | ScoreLevel5
    | ScoreLevel6
    | ScoreLevel7
    | ScoreLevel8
    | ScoreLevel9
