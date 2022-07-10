module DesignSystem.Theme exposing
    ( Theme
    , ColorScheme, ColorPair
    , Rythm, defaultTheme
    )

{-| This module contains the theme(s) for the app.


# Theme

@docs Theme, deaultTheme


# Color

@docs ColorScheme, ColorPair

-}

import DesignSystem.Responsiveness exposing (rythm)
import Element exposing (Color)



--------------------------------------------------------------------------------
-- Theme
--------------------------------------------------------------------------------


type alias Theme =
    { colorScheme : ColorScheme
    , fontRythm : Rythm
    , lineHeightRythm : Rythm
    , spacingRythm : Rythm
    , borderRadiusRythm : Rythm
    , shadow : Shadow
    }


defaultTheme : Theme
defaultTheme =
    { colorScheme = lightColorScheme
    , fontRythm = fontRythm
    , spacingRythm = spacingRythm
    , lineHeightRythm = lineHeightRythm
    , borderRadiusRythm = borderRadiusRythm
    , shadow = shadow
    }



--------------------------------------------------------------------------------
-- Color
--------------------------------------------------------------------------------


{-| A color scheme that makes it easy to switch out the colors used throught out
the app
-}
type alias ColorScheme =
    { primaryBrand : ColorPair
    , secondaryBrand : ColorPair
    , dark : ColorPair
    , success : ColorPair
    , failure : ColorPair
    , textBody : Color
    , textSubtle : Color
    , accent : Color
    , background : Color
    , nftState :
        { available : ColorPair
        , reserved : ColorPair
        , sold : ColorPair
        , error : ColorPair
        , other : ColorPair
        }
    , gradient :
        { angle : Float
        , steps : List Color
        }
    , black : Color
    , white : Color
    }


{-| Pairs of color and the color to use on text displayed on top of it.
-}
type alias ColorPair =
    { color : Color
    , text : Color
    }


colorPair : Color -> Color -> ColorPair
colorPair color text =
    { color = color, text = text }


white : Color
white =
    Element.rgb255 255 255 255


black : Color
black =
    Element.rgb255 0 0 0


{-| The light theme is the default color scheme used by this app.
-}
lightColorScheme : ColorScheme
lightColorScheme =
    let
        primaryBrand =
            colorPair (Element.rgb255 123 97 255) white

        secondaryBrand =
            colorPair (Element.rgb255 42 217 143) white

        success =
            colorPair (Element.rgb255 0 224 170) white

        failure =
            colorPair (Element.rgb255 211 0 89) white

        textBody =
            Element.rgb255 74 74 104

        accent =
            Element.rgb255 236 241 244
    in
    { primaryBrand = primaryBrand
    , secondaryBrand = secondaryBrand
    , dark = colorPair (Element.rgb255 57 37 72) white
    , success = success
    , failure = failure
    , textBody = textBody
    , textSubtle = Element.rgb255 129 129 153
    , accent = accent
    , background = white
    , nftState =
        { available = colorPair (Element.rgb255 1 181 139) white
        , sold = primaryBrand
        , reserved = colorPair (Element.rgb255 236 164 0) white
        , error = failure
        , other = colorPair accent textBody
        }
    , gradient =
        { angle = pi / 2
        , steps =
            [ primaryBrand.color
            , secondaryBrand.color
            ]
        }
    , white = white
    , black = black
    }



--------------------------------------------------------------------------------
-- Shadow
--------------------------------------------------------------------------------


type alias Shadow =
    { offset : ( Float, Float )
    , size : Float
    , blur : Float
    , color : Color
    }


shadow : Shadow
shadow =
    { offset = ( 0, 0 )
    , size = 0
    , blur = 16
    , color = Element.rgba255 0 0 0 0.2
    }



--------------------------------------------------------------------------------
-- Sizes & Rythm
--------------------------------------------------------------------------------


type alias Rythm =
    Int -> Float


{-| The fontRythm provides geometrically scale of font sizes.

This is usefull to create a natural ordering of fonts sizes that are pleasing to the viwer.

-}
fontRythm : Rythm
fontRythm =
    rythm 16 1.25


lineHeightRythm : Rythm
lineHeightRythm =
    rythm 18 1.25


{-| The spacingRythm provides geometrically scale of spacing sizes.

This is usefull to create a natural ordering of spacing sizes that are pleasing to the viwer.

-}
spacingRythm : Rythm
spacingRythm =
    rythm 4 2.0


{-| The borderRadiusRythm provides geometrically scale of radius sizes.

This is usefull to create a natural ordering of radius sizes that are pleasing to the viwer.

-}
borderRadiusRythm : Rythm
borderRadiusRythm =
    rythm 4 2.0
