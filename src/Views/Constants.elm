module Views.Constants exposing (..)

import Element exposing (Attribute, rgb255)
import Element.Font as Font


blueBgColor =
    rgb255 63 191 255


grayBgColor =
    rgb255 223 223 223


grayFgColor =
    rgb255 127 127 127



-- alternative colors, with (mostly) the same saturation as the blue


purpleColor =
    rgb255 191 63 255


greenColor =
    rgb255 143 191 47


redColor =
    rgb255 255 63 63


orangeColor =
    rgb255 255 159 63


yellowColor =
    rgb255 255 255 63


{-| Enough alternative colors to assign one to each friend such that: most of the time, each friend
gets a unique color; when you have a lot of friends, no two friends have the same color _and_ the
same initial.
-}
friendColors =
    [ purpleColor
    , redColor
    , orangeColor
    , greenColor
    ]


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
