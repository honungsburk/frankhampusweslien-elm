module Components.Hairline exposing (hairline)

import DesignSystem.Theme exposing (Theme)
import Element exposing (..)
import Element.Border as Border
import Html


hairline : Theme -> Int -> Element msg
hairline theme n =
    el
        [ Border.color theme.colorScheme.accent
        , Border.widthEach { bottom = n, left = 0, right = 0, top = 0 }
        , Border.solid
        , Element.width Element.fill
        ]
    <|
        Element.html <|
            Html.span [] []
