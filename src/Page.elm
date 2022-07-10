module Page exposing (Page(..), view)

import DesignSystem exposing (DesignSystem)
import DesignSystem.Fonts exposing (roboto)
import DesignSystem.Responsiveness exposing (Device)
import DesignSystem.Theme exposing (Theme)
import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Font as Font
import Element.Lazy as Lazy
import Element.Region as Region exposing (description)
import FeatherIcons
import Icons
import Route
import Session exposing (Session)



--------------------------------------------------------------------------------
-- Page
--------------------------------------------------------------------------------


{-| Represents which page in the header we are on. Other is for any page not
available in the header.
-}
type Page
    = Other
    | FAQ
    | Home
    | Art
    | Me


{-| Take a page's Html and frames it with a header and footer.
-}
view : Session -> Page -> { title : String, content : Element msg } -> { title : String, body : Element msg }
view session page { title, content } =
    { title = title ++ " - Frank Hampus Weslien"
    , body =
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ session.nftMakerConnectionError
                |> Maybe.map (connectionStatus session.designSystem.theme)
                |> Maybe.withDefault Element.none
            , Lazy.lazy2 headerView session.designSystem page
            , Element.el
                [ Element.width Element.fill
                , Region.mainContent
                ]
                content
            , Lazy.lazy footerView session.designSystem
            ]
    }



--------------------------------------------------------------------------------
-- Connection Status
--------------------------------------------------------------------------------


connectionStatus : Theme -> String -> Element msg
connectionStatus theme msg =
    Element.paragraph
        [ Background.color theme.colorScheme.failure.color
        , roboto
        , Font.medium
        , Font.size << round <| theme.fontRythm -1
        , Font.color theme.colorScheme.failure.text
        , Element.width Element.fill
        , Font.center
        ]
        [ Element.text msg
        ]



--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------


headerView : DesignSystem -> Page -> Element msg
headerView designSystem page =
    let
        isMobile =
            designSystem.device.class == DesignSystem.Responsiveness.Phone
    in
    Element.row
        [ Element.width Element.fill
        , Element.paddingXY
            (Basics.round <| designSystem.theme.spacingRythm 3)
            (Basics.round <| designSystem.theme.spacingRythm 2)
        , Region.navigation
        ]
        [ Element.link
            [ Font.regular
            , Font.size << Basics.round <| designSystem.theme.fontRythm 4
            , roboto
            , Font.color designSystem.theme.colorScheme.dark.color
            ]
            { url = Route.routeToPath Route.Home
            , label =
                Element.image
                    [ Element.height
                        << Element.px
                        << Basics.round
                        << designSystem.theme.fontRythm
                      <|
                        if isMobile then
                            2

                        else
                            4
                    ]
                    { description = "FHW"
                    , src = "/assets/icons/FHW-logo-large.svg"
                    }
            }
        , Element.row
            [ Element.alignRight
            , Element.spacing (Basics.round <| designSystem.theme.spacingRythm 2)
            ]
            [ headerLink designSystem Route.initArt page
            , headerLink designSystem Route.FAQ page
            , headerLink designSystem Route.Me page
            ]
        ]


headerLink : DesignSystem -> Route.Route -> Page -> Element msg
headerLink designSystem route currentPage =
    let
        font =
            [ Font.light
            , Font.center
            , Font.size << Basics.round <| designSystem.theme.fontRythm 3
            , roboto
            , Font.color designSystem.theme.colorScheme.dark.color
            ]
    in
    Element.link
        (if isActive route currentPage then
            Font.underline :: font

         else
            font
        )
        { url = Route.routeToPath route
        , label = Element.text <| Route.routeToString route
        }


isActive : Route.Route -> Page -> Bool
isActive route page =
    case ( page, route ) of
        ( FAQ, Route.FAQ ) ->
            True

        ( Art, Route.Art _ ) ->
            True

        ( Me, Route.Me ) ->
            True

        _ ->
            False



--------------------------------------------------------------------------------
-- Footer
--------------------------------------------------------------------------------


footerView : DesignSystem -> Element msg
footerView { theme } =
    let
        linkSize =
            theme.fontRythm 2
    in
    Element.row
        [ Element.spacing << Basics.round <| theme.spacingRythm 1
        , Element.centerX
        , Element.alignBottom
        , Element.padding << Basics.round <| theme.spacingRythm 2
        , Font.color theme.colorScheme.dark.color
        , Region.footer
        ]
        [ socialMediaLink
            linkSize
            "https://www.linkedin.com/in/frank-hampus-weslien-b81b59197/"
            "My linkedin page"
            FeatherIcons.linkedin
        , socialMediaLink
            linkSize
            "https://www.instagram.com/frankhampusweslien/"
            "My instagram page"
            FeatherIcons.instagram
        , socialMediaLink
            linkSize
            "https://twitter.com/HampusFrank"
            "My twitter page"
            FeatherIcons.twitter
        , socialMediaLink
            linkSize
            "https://www.youtube.com/channel/UC6fuoBfK8_B_cT35aKuJEgg"
            "My youtube page"
            FeatherIcons.youtube
        ]


socialMediaLink : Float -> String -> String -> FeatherIcons.Icon -> Element msg
socialMediaLink size url description icon =
    Element.link
        [ Region.description description
        ]
        { url = url
        , label = Icons.toElement size icon
        }
