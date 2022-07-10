module Asset exposing
    ( Image
    , collage
    , src
    )

{-| Assets, such as images, videos, and audio. (We only have images for now.)

We should never expose asset URLs directly; this module should be in charge of
all of them. One source of truth!

-}


type Image
    = PageAsset String



--------------------------------------------------------------------------------
-- Images
--------------------------------------------------------------------------------


collage : Image
collage =
    pageAsset "collage-improved.png"


pageAsset : String -> Image
pageAsset filename =
    PageAsset ("/assets/page/" ++ filename)



--------------------------------------------------------------------------------
-- Using Images
--------------------------------------------------------------------------------


src : Image -> String
src (PageAsset url) =
    url
