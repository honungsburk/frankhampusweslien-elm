module DesignSystem.Responsiveness exposing (..)

{-| This module contains helper functions to support a responsive design.

Look at for how to get the data:
<https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#classifyDevice>


# Screen

@docs Device, DeviceClass, Orientation, classifyDevice


## Devices

@docs phoneL, phoneP, tabletL, tabletP, desktopL, desktopP, bigDesktopL, bigDesktopP


# Grid

@docs Grid, initGrid


# Rythm

@docs modular

-}


{-| Represents the screen of the device. Useful to determine the most optimal
layout.
-}
type alias Device =
    { class : DeviceClass
    , orientation : Orientation
    }


{-| Represents the different devices for which the layout can be adopted for.
-}
type DeviceClass
    = Phone
    | Tablet
    | Desktop
    | BigDesktop


{-| Representes the orientation of the screen
-}
type Orientation
    = Portrait
    | Landscape


{-| Takes in a Window.Size and returns a device profile which can be used for responsiveness.
-}
classifyDevice : { window | height : Int, width : Int } -> Device
classifyDevice window =
    { class =
        let
            longSide =
                max window.width window.height

            shortSide =
                min window.width window.height
        in
        if shortSide < 600 then
            Phone

        else if longSide <= 1200 then
            Tablet

        else if longSide > 1200 && longSide <= 1920 then
            Desktop

        else
            BigDesktop
    , orientation =
        if window.width < window.height then
            Portrait

        else
            Landscape
    }



--------------------------------------------------------------------------------
-- Grid
--------------------------------------------------------------------------------


{-| Keep track of the grid system to use on the current device.
-}
type alias Grid =
    { paddingX : Int
    , paddingY : Int
    , spacingX : Int
    , spacingY : Int
    , totalColumns : Int
    }


{-| Depending on the Device will produce a suitable Grid.
-}
initGrid : Device -> Grid
initGrid device =
    case ( device.class, device.orientation ) of
        ( Phone, Portrait ) ->
            { paddingX = 24
            , paddingY = 0
            , spacingX = 16
            , spacingY = 16
            , totalColumns = 4
            }

        ( Phone, Landscape ) ->
            { paddingX = 32
            , paddingY = 0
            , spacingX = 16
            , spacingY = 16
            , totalColumns = 8
            }

        ( Tablet, Portrait ) ->
            { paddingX = 32
            , paddingY = 0
            , spacingX = 16
            , spacingY = 16
            , totalColumns = 8
            }

        ( Tablet, Landscape ) ->
            { paddingX = 32
            , paddingY = 0
            , spacingX = 16
            , spacingY = 16
            , totalColumns = 12
            }

        ( Desktop, Portrait ) ->
            { paddingX = 40
            , paddingY = 0
            , spacingX = 16
            , spacingY = 16
            , totalColumns = 8
            }

        ( Desktop, Landscape ) ->
            { paddingX = 64
            , paddingY = 0
            , spacingX = 24
            , spacingY = 24
            , totalColumns = 12
            }

        ( BigDesktop, Portrait ) ->
            { paddingX = 64
            , paddingY = 0
            , spacingX = 24
            , spacingY = 24
            , totalColumns = 12
            }

        ( BigDesktop, Landscape ) ->
            { paddingX = 128
            , paddingY = 0
            , spacingX = 24
            , spacingY = 24
            , totalColumns = 16
            }



--------------------------------------------------------------------------------
-- rythm
--------------------------------------------------------------------------------


{-| When designing it's nice to use a modular scale to set spacial rythms.
-}
rythm : Float -> Float -> Int -> Float
rythm base per scale =
    base * (per ^ toFloat scale)
