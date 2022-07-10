module DesignSystem.Fonts exposing (roboto)

{-| This moduel contains all the fonts used thourghout the app. Important to note
is that the fonts are specified with rythmic sizes and can very easily be scaled
to fit the screen.
-}

import Element exposing (Attribute)
import Element.Font exposing (..)


roboto : Attribute msg
roboto =
    family
        [ typeface "Roboto"
        , sansSerif
        ]
