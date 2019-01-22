module Constants exposing
    ( backgroundColour
    , bodyFontSize
    , bodyPadding
    , cardBackColour
    , cardCornerRound
    , cardHeight
    , cardSpaceColour
    , cardWidth
    , clubsColour
    , columnWarningColour
    , diamondsColour
    , heartsColour
    , sidebarFontColour
    , spadesColour
    , starsColour
    )

import Element exposing (..)



-- Colours


columnWarningColour : Color
columnWarningColour =
    rgb255 250 100 42


backgroundColour : Color
backgroundColour =
    rgb255 157 120 85


sidebarFontColour : Color
sidebarFontColour =
    rgb 1 1 1


bodyFontSize : Int
bodyFontSize =
    75


bodyPadding : Int
bodyPadding =
    10


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


cardBackColour : Color
cardBackColour =
    rgb255 44 49 64


cardCornerRound : Int
cardCornerRound =
    4


cardHeight : Int
cardHeight =
    105


cardWidth : Int
cardWidth =
    68


cardSpaceColour : Color
cardSpaceColour =
    rgba 0 0 0 0.25
