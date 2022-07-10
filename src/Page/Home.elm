module Page.Home exposing (view)

import Asset
import Components.Hairline
import DesignSystem exposing (DesignSystem)
import DesignSystem.Fonts exposing (roboto)
import DesignSystem.Responsiveness as Responsiveness
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy as Lazy
import FeatherIcons as Feather exposing (Icon)
import Icons
import Route



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : DesignSystem -> { title : String, content : Element msg }
view designSystem =
    { title = "Home"
    , content = Lazy.lazy home designSystem
    }


home : DesignSystem -> Element msg
home designSystem =
    Element.column
        [ Element.centerX
        , Element.spacing << round <| designSystem.theme.spacingRythm 4
        ]
        [ hero designSystem
        , Components.Hairline.hairline designSystem.theme 4
        , quickExplainers designSystem
        ]


{-| The purpose of the hero page is for the user to immeditaly answer the
follwoing questions:

1.  What is it?
2.  Is it right for me?
3.  Is it legit? (other stuff on website helps with this too)

-}
hero : DesignSystem -> Element msg
hero designSystem =
    let
        colorScheme =
            designSystem.theme.colorScheme

        maxHeight =
            Basics.min
                (designSystem.fractionOfHeight 0.7)
                (designSystem.widthOfNCol designSystem.grid.totalColumns)
    in
    Element.column
        [ Element.centerX
        , Element.spacing (round <| designSystem.theme.spacingRythm 3)
        , Element.width << Element.px <| designSystem.widthOfNCol 4
        ]
        [ Element.image
            [ Element.width <| Element.maximum maxHeight Element.fill
            , Element.centerX
            ]
            { src = Asset.src Asset.collage
            , description = "Collage of some of Frank's digital art"
            }
        , Element.column
            [ Element.centerX
            , Element.spacing << round <| designSystem.theme.spacingRythm 2
            ]
            [ Element.paragraph
                [ Font.color colorScheme.dark.color
                , Font.center
                , roboto
                , Font.size << round <| designSystem.theme.fontRythm 4
                ]
                [ Element.text "Collect fine art on the Cardano blockchain" ]
            , customerValueBar designSystem
            , Element.link
                [ Element.centerX
                , Background.gradient colorScheme.gradient
                , Font.color colorScheme.white
                , Element.paddingXY
                    (round <| designSystem.theme.spacingRythm 3)
                    (round <| designSystem.theme.spacingRythm 1)
                , Border.rounded 32
                , Font.size << round <| designSystem.theme.fontRythm 2
                ]
                { url = Route.routeToPath Route.initArt
                , label = Element.text "Explore"
                }
            ]
        ]


customerValueBar : DesignSystem -> Element msg
customerValueBar designSystem =
    let
        colorScheme =
            designSystem.theme.colorScheme

        customerValue : Icon -> String -> Element msg
        customerValue icon s =
            Element.row
                [ Element.spacing << round <| designSystem.theme.spacingRythm 1
                ]
                [ Element.el
                    [ Font.color colorScheme.primaryBrand.text
                    , Background.color colorScheme.primaryBrand.color
                    , Element.padding << round <| designSystem.theme.spacingRythm 1
                    , Border.rounded 32
                    ]
                  <|
                    Icons.toElement (designSystem.theme.fontRythm 1) icon
                , Element.el
                    [ Font.color colorScheme.textBody
                    , roboto
                    ]
                  <|
                    Element.text s
                ]

        layout =
            if designSystem.device.class == Responsiveness.Phone then
                Element.column
                    [ Element.spacing << round <| designSystem.theme.spacingRythm 2
                    , Element.centerX
                    ]

            else
                Element.row
                    [ Element.spacing << round <| designSystem.theme.spacingRythm 3
                    , Element.centerX
                    ]
    in
    layout
        [ customerValue Feather.layers "Collect & Hold"
        , customerValue Feather.gift "Share & Gift"
        , customerValue Feather.refreshCw "Buy & Sell"
        ]


quickExplainers : DesignSystem -> Element msg
quickExplainers designSystem =
    let
        theme =
            designSystem.theme

        explainer : String -> String -> Element msg
        explainer title body =
            Element.column
                [ Element.centerX
                , Element.spacing << round <| designSystem.theme.spacingRythm 2
                ]
                [ Element.el
                    [ Font.size << round <| theme.fontRythm 3
                    , Font.color theme.colorScheme.dark.color
                    , Font.center
                    , Element.centerX
                    ]
                  <|
                    Element.text title
                , Element.paragraph
                    [ Element.width << Element.px <| designSystem.widthOfNCol 4
                    , Font.size << round <| theme.fontRythm 0
                    , Font.center
                    , Font.color theme.colorScheme.textBody
                    ]
                    [ Element.text body
                    ]
                ]
    in
    Element.column
        [ Element.centerX
        , Element.spacing << round <| theme.spacingRythm 4
        ]
        [ explainer
            "What is a NFT?"
          <|
            ("A NFT (Non-Fungiable Token) is an asset stored on a blockshain. "
                ++ "It has one crucial property: each one is unique. "
                ++ "In short, it lets us own stuff on the internet; "
                ++ "A perfect fit for a digital artist."
            )
        , explainer
            "Why should I buy one?"
            "You like my art, you like what I do, and you want my art to be part of your private collection."
        , explainer
            "Still clueless?"
            "Reach out on"
        ]
