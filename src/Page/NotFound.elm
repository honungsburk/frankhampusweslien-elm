module Page.NotFound exposing (view)

import DesignSystem exposing (DesignSystem)
import DesignSystem.Theme exposing (Theme)
import Element exposing (Element, el, text)
import Element.Font as Font
import Element.Lazy as Lazy



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : DesignSystem -> { title : String, content : Element msg }
view designSystem =
    { title = "Page Not Found"
    , content = Lazy.lazy notFound designSystem
    }


notFound : DesignSystem -> Element msg
notFound designSystem =
    el
        [ Font.size << Basics.round <| designSystem.theme.fontRythm 6
        , Font.extraBold
        , Font.bold
        , Font.center
        , Font.family
            [ Font.typeface "Roboto"
            ]
        , Element.centerX
        , Element.centerY
        , Font.color designSystem.theme.colorScheme.dark.color
        ]
    <|
        text "404 Not Found"
