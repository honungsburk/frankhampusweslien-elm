module Page.Me exposing (view)

import Art.Doc
import DesignSystem exposing (DesignSystem)
import Element exposing (Element)
import Element.Font as Font
import Element.Lazy as Lazy
import Route



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : DesignSystem -> { title : String, content : Element msg }
view designSystem =
    { title = "Me"
    , content = Lazy.lazy me designSystem
    }


me : DesignSystem -> Element msg
me designSystem =
    let
        fourCol =
            Element.px <| designSystem.widthOfNCol 4

        theme =
            designSystem.theme
    in
    Element.column
        [ Element.width fourCol
        , Element.centerX
        , Element.paddingXY 0 <| Basics.round <| designSystem.theme.spacingRythm 3
        , Element.spacing <| Basics.round <| designSystem.theme.spacingRythm 2
        ]
        [ Element.image
            [ Element.width fourCol
            ]
            { src = Maybe.withDefault "lol" <| Art.Doc.lowResSrc Art.Doc.selfPortraitTwo -- TODO: Fix better fall back
            , description = "A self portrait of and by Frank Hampus Weslien."
            }
        , Element.paragraph
            [ Font.size << Basics.round <| designSystem.theme.fontRythm 0
            , Font.color theme.colorScheme.textBody
            , Font.family
                [ Font.typeface "Roboto"
                ]
            ]
            [ Element.text "Hi! I'm Frank Hampus Weslien a professional developer and artist. "
            , Element.text "And what better way to bring those two together then through NFT:s? "
            , Element.text "I always have something in works to soon be revealed; check out my twitter or instagram if you want updates. "
            , Element.text "In the mean time you can check out some of my previous "
            , Element.link
                [ Font.underline, Font.color theme.colorScheme.dark.color ]
                { url = Route.routeToPath Route.initArt
                , label = Element.text "work"
                }
            , Element.text ". "
            , Element.text "- Cheers"
            ]
        ]
