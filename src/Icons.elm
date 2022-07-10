module Icons exposing (..)

import Element exposing (Element)
import FeatherIcons


{-| Helper function to turn FeatherIcons.Icon into Elm-UI elements.

    toElement 16 instagram

-}
toElement : Float -> FeatherIcons.Icon -> Element msg
toElement size icon =
    icon
        |> FeatherIcons.withSize size
        |> FeatherIcons.toHtml []
        |> Element.html
