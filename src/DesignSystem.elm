module DesignSystem exposing (DesignSystem, mkDesignSystem)

{-| This module contains data types used to allow for changes in the design
system to permutate through the app.


# Design System

@docs DesignSystem, mkDesignSystem, widthOfNCol

-}

import DesignSystem.Responsiveness as Resp exposing (Device, Grid)
import DesignSystem.Theme exposing (Theme, defaultTheme)


type alias DesignSystem =
    { theme : Theme
    , device : Device
    , grid : Grid
    , widthOfNCol : Int -> Int
    , fractionOfHeight : Float -> Int
    }


{-| Use the dimensions of the screen to figure out the best Design System to use.
-}
mkDesignSystem : { window | height : Int, width : Int } -> DesignSystem
mkDesignSystem record =
    let
        device =
            Resp.classifyDevice record

        columns =
            Resp.initGrid device
    in
    { theme = defaultTheme
    , device = device
    , grid = Resp.initGrid device
    , widthOfNCol = widthOfNCol record.width columns
    , fractionOfHeight = fractionOfHeight record.height
    }


{-| Computes the width of an element that would span a number of columns.
Note that this includes the spacing between columns as well. It allows you, for
example, match the width and height of an element.
-}
widthOfNCol : Int -> Grid -> Int -> Int
widthOfNCol width { paddingX, spacingX, totalColumns } columnsToSpan =
    let
        oneColumnsWidth =
            toFloat (width - paddingX * 2 - (totalColumns - 1) * spacingX) / toFloat totalColumns
    in
    round oneColumnsWidth * columnsToSpan + (columnsToSpan - 1) * spacingX


fractionOfHeight : Int -> Float -> Int
fractionOfHeight height l =
    Basics.round <| l * toFloat height
