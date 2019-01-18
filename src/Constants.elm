module Constants exposing
    ( backgroundColour
    , cardSpaceBackground
    , clubsColour
    , columnWarningColour
    , diamondsColour
    , heartsColour
    , spadesColour
    , starsColour
    )

import Element exposing (..)


heartsColour : Color
heartsColour =
    rgb255 218 87 53


clubsColour : Color
clubsColour =
    rgb255 114 147 181


diamondsColour : Color
diamondsColour =
    rgb255 242 168 31


spadesColour : Color
spadesColour =
    rgb255 54 55 36


starsColour : Color
starsColour =
    rgb255 109 167 128


columnWarningColour : Color
columnWarningColour =
    rgb255 250 100 42


backgroundColour : Color
backgroundColour =
    rgb255 157 120 85


cardSpaceBackground : Color
cardSpaceBackground =
    rgba 0 0 0 0.25
