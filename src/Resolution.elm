module Resolution exposing (..)

{-| To be able to know the best way to display an image it is best if we know
the resolution. Then we know if we should down sample the image and how to optimize
the layout.


# Resolution

@docs Resolution, resolution, isInPortrait, isInPortrait

-}


type alias Resolution =
    { width : Int, height : Int }


resolution : Int -> Int -> Resolution
resolution width height =
    { width = width, height = height }


{-| Check if the Resolution means the image is in portrait mode.
-}
isInPortrait : Resolution -> Bool
isInPortrait { width, height } =
    height > width


{-| Check if the Resolution means the image is in landscape mode.
-}
isInLandscape : Resolution -> Bool
isInLandscape { width, height } =
    height < width
