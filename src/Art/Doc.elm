module Art.Doc exposing
    ( Doc
    , thumbNailSrc, lowResSrc, fullResSrc
    , displayName
    , artIndex, makeLookupIndex, forSale, hasGroup
    , SaleStateCounts, countSaleStates, initSaleStateCounts
    , artContent, selfPortraitTwo
    , safetyCheck
    )

{-| This module contains types for describing and indexing art.


# Doc

@docs Doc


# Src

@docs thumbNailSrc, lowResSrc, fullResSrc


# Name

@docs displayName


# Search

@docs artIndex, makeLookupIndex, forSale, hasGroup


# Count Sale States

@docs SaleStateCounts, countSaleStates, initSaleStateCounts


# Content

@docs artContent, selfPortraitTwo

-}

import Art.Group exposing (Group)
import Art.Sale as Sale
import Derberos.Date.Core exposing (civilToPosix, newDateRecord)
import DesignSystem.Theme
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font as Font
import ElmTextSearch
import Expect exposing (true)
import MimeType exposing (MimeImage(..), MimeType(..), MimeVideo(..))
import Resolution exposing (resolution)
import Set
import Time exposing (Posix)



--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------


{-| Holds all metadata related to an individual art piece.

TODO: Make invalid states unrepresentable!!!

-}
type alias Doc =
    { cid : String
    , name : String
    , description : List (Element ())
    , shortDescription : String
    , group : Group
    , mime : MimeType
    , fileName : String
    , tags : List String
    , buyStatus : Sale.Status
    , createdAt : Posix
    , resolution : Resolution.Resolution
    }



--------------------------------------------------------------------------------
-- Src
--------------------------------------------------------------------------------


{-| The absolut filepath to a thumb nail version of the image.
-}
thumbNailSrc : Doc -> Maybe String
thumbNailSrc doc =
    case doc.group of
        Art.Group.Painting ->
            Maybe.map (\s -> Art.Group.path doc.group ++ s) <| extendFileName "_Thumb_Nail." doc.fileName

        Art.Group.StainedGlass _ ->
            Maybe.map (\s -> Art.Group.path doc.group ++ "thumbnails/" ++ s) <| replaceExtention doc.fileName "png"

        Art.Group.AlgoMarble ->
            Maybe.map (\s -> Art.Group.path doc.group ++ "thumbnails/" ++ s) <| extendFileName "_Thumb_Nail." doc.fileName

        Art.Group.Motion ->
            Just <| Art.Group.path doc.group ++ "thumbnails/" ++ doc.fileName


{-| The absolut filepath to a low resolution version of the image.
-}
lowResSrc : Doc -> Maybe String
lowResSrc doc =
    case doc.group of
        Art.Group.Painting ->
            Maybe.map (\s -> Art.Group.path doc.group ++ s) <| extendFileName "_low_res." doc.fileName

        Art.Group.StainedGlass _ ->
            Just <| fullResSrc doc

        Art.Group.AlgoMarble ->
            Maybe.map (\s -> Art.Group.path doc.group ++ "low-res/" ++ s) <| extendFileName "_low_res." doc.fileName

        Art.Group.Motion ->
            Just <| Art.Group.path doc.group ++ doc.fileName


{-| The absolut filepath to the highest resolution version of the image.
-}
fullResSrc : Doc -> String
fullResSrc { group, fileName } =
    let
        bucket =
            "https://storage.googleapis.com/frankhampusweslien.com/"
    in
    case group of
        Art.Group.AlgoMarble ->
            bucket ++ "algomarble/high-res/" ++ fileName

        Art.Group.StainedGlass _ ->
            Art.Group.path group ++ fileName

        Art.Group.Painting ->
            Art.Group.path group ++ fileName

        Art.Group.Motion ->
            bucket ++ "motion/" ++ (Maybe.withDefault fileName <| replaceExtention fileName "mp4")


extendFileName : String -> String -> Maybe String
extendFileName extra fileName =
    case String.split "." fileName of
        [ name, extention ] ->
            Just <| name ++ extra ++ extention

        _ ->
            Nothing


replaceExtention : String -> String -> Maybe String
replaceExtention oldFile newExtention =
    case String.split "." oldFile of
        [ name, _ ] ->
            Just <| name ++ "." ++ newExtention

        _ ->
            Nothing



--------------------------------------------------------------------------------
-- Name
--------------------------------------------------------------------------------


{-| The name to be shown to the user in the GUI for a specific NFT
-}
displayName : Doc -> String
displayName doc =
    case doc.group of
        Art.Group.Painting ->
            doc.name

        Art.Group.AlgoMarble ->
            "#" ++ doc.name

        Art.Group.Motion ->
            "MOTION-" ++ doc.name

        Art.Group.StainedGlass mName ->
            case mName.communityName of
                Nothing ->
                    "#" ++ doc.name

                Just communityName ->
                    "#" ++ doc.name ++ " (" ++ communityName ++ ")"



--------------------------------------------------------------------------------
-- Search
--------------------------------------------------------------------------------


{-| Determines if an ArtDoc is available for sale.
-}
forSale : Doc -> Bool
forSale =
    .buyStatus
        >> Sale.available


{-| Determine if a Doc belongs to a certain Group.
-}
hasGroup : Art.Group.Group -> Doc -> Bool
hasGroup g doc =
    Art.Group.sameGroup g doc.group


{-| The index contains all the art pieces can be searched. If there was an error
while indexing the List will be non-empty. The int is the index which got an error
and the String is the error message.
-}
artIndex : ( ElmTextSearch.Index Doc, List ( Int, String ) )
artIndex =
    let
        initIndex : ElmTextSearch.Index Doc
        initIndex =
            ElmTextSearch.new
                { ref = .cid
                , fields =
                    [ ( displayName, 5.0 )
                    , ( MimeType.toIndexable << .mime, 1.0 )
                    ]
                , listFields = [ ( .tags, 2.0 ) ]
                }
    in
    ElmTextSearch.addDocs artContent initIndex


{-| A simple index used to lookup ArtDoc:s based on their ID:s
-}
makeLookupIndex : List Doc -> Dict String Doc
makeLookupIndex =
    Dict.fromList << List.map (\art -> ( art.cid, art ))



--------------------------------------------------------------------------------
-- NFT State Counts
--------------------------------------------------------------------------------


type alias SaleStateCounts =
    { total : Int
    , available : Int
    , reserved : Int
    , sold : Int
    , error : Int
    , notForSale : Int
    }


initSaleStateCounts : SaleStateCounts
initSaleStateCounts =
    SaleStateCounts 0 0 0 0 0 0


countSaleStates : List { doc | buyStatus : Sale.Status } -> SaleStateCounts
countSaleStates =
    let
        count : { doc | buyStatus : Sale.Status } -> SaleStateCounts -> SaleStateCounts
        count doc state =
            let
                stateNewTotal =
                    { state
                        | total = state.total + 1
                    }
            in
            case doc.buyStatus of
                Sale.Gifted ->
                    { stateNewTotal | sold = state.sold + 1 }

                Sale.Bought _ ->
                    { stateNewTotal | sold = state.sold + 1 }

                Sale.DMIfInterested ->
                    { stateNewTotal | available = state.available + 1 }

                Sale.NotForSale ->
                    { stateNewTotal | notForSale = state.notForSale + 1 }
    in
    List.foldl count initSaleStateCounts



--------------------------------------------------------------------------------
-- Content
--------------------------------------------------------------------------------


mkCreatedAt : Int -> Int -> Int -> Posix
mkCreatedAt year month day =
    civilToPosix <| newDateRecord year month day 0 0 0 0 Time.utc


{-| All artworks present on the site.
-}
artContent : List Doc
artContent =
    paintings
        ++ stainedGlass
        ++ algoMarble
        ++ motion
        |> List.sortBy (Time.posixToMillis << .createdAt)
        |> List.reverse
        |> safetyCheck


{-| Returns an empty list if two Doc names collide-
Why? Because the names are used in NFTMaker to mint NFTs
and it would be really bad if the wrong nft got minted.
-}
safetyCheck : List { doc | name : String } -> List { doc | name : String }
safetyCheck art =
    let
        withCollisions =
            Set.fromList <| List.map .name art
    in
    if List.length art == Set.size withCollisions then
        art

    else
        []


{-| Used on the /me page
-}
selfPortraitTwo : Doc
selfPortraitTwo =
    autoCid
        { name = "Self Portrait #2"
        , description = "A self portrait."
        , mime = Image Jpeg
        , fileName = "Self_Portrait_2.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Portrait" ]
        , buyStatus = Sale.notForSale
        , createdAt = mkCreatedAt 2020 1 12
        , resolution = resolution 2920 3644
        }


autoCid doc =
    { cid = String.replace " " "-" <| String.filter ((/=) '#') doc.name
    , name = doc.name
    , description = [ Element.paragraph [] [ Element.text doc.description ] ]
    , shortDescription = doc.description
    , mime = doc.mime
    , fileName = doc.fileName
    , group = doc.group
    , tags = doc.tags
    , buyStatus = doc.buyStatus
    , createdAt = doc.createdAt
    , resolution = doc.resolution
    }


paintings : List Doc
paintings =
    [ autoCid
        { name = "This Is Not A Banana"
        , description = """A banana version of "The Trechery Of Images" by René Magritte."""
        , mime = Image Jpeg
        , fileName = "This_Is_Not_A_Banana.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Banana", "Surrealism" ]
        , buyStatus = Sale.bought
        , createdAt = mkCreatedAt 2020 9 4
        , resolution = resolution 5500 4000
        }
    , autoCid
        { name = "Europe"
        , description = "The European flag re-imagined."
        , mime = Image Jpeg
        , fileName = "European_Flowers.jpg"
        , group = Art.Group.painting
        , tags = [ "Re-imagined Flags", "Procreate" ]
        , buyStatus = Sale.boughtFor 50
        , createdAt = mkCreatedAt 2021 8 8
        , resolution = resolution 6000 4000
        }
    , autoCid
        { name = "Japan"
        , description = "The Japanese flag re-imagined. It takes heavy inspiration from tattoo which I thought looked a lot like the flag of Japan."
        , mime = Image Jpeg
        , fileName = "Japan_Remade.jpg"
        , group = Art.Group.painting
        , tags = [ "Re-imagined Flags", "Procreate" ]
        , buyStatus = Sale.boughtFor 100
        , createdAt = mkCreatedAt 2021 8 7
        , resolution = resolution 6000 4000
        }
    , autoCid
        { name = "Sketch Of A Boy"
        , description = "A Simple sketch I did."
        , mime = Image Jpeg
        , fileName = "30_min_sketch.jpg"
        , group = Art.Group.painting
        , tags = [ "Portrait", "Sketch", "Krita" ]
        , buyStatus = Sale.boughtFor 50
        , createdAt = mkCreatedAt 2020 12 1
        , resolution = resolution 2048 2732
        }
    , autoCid
        { name = "Rising Bull"
        , description = "A bull rising with the sun."
        , mime = Image Jpeg
        , fileName = "Bull.jpg"
        , group = Art.Group.painting
        , tags = [ "Bull", "Krita", "Animal" ]
        , buyStatus = Sale.boughtFor 100
        , createdAt = mkCreatedAt 2021 5 1
        , resolution = resolution 2048 2732
        }
    , autoCid
        { name = "Canyon River"
        , description = "This is a copy of another artist's work. It was done for practice."
        , mime = Image Jpeg
        , fileName = "Canyon_River.jpg"
        , group = Art.Group.painting
        , tags = [ "Derivative", "Krita", "Landscape" ]
        , buyStatus = Sale.notForSale
        , createdAt = mkCreatedAt 2019 5 5
        , resolution = resolution 1200 1920
        }
    , autoCid
        { name = "Cliff and Ocean"
        , description = "My first attempt at a landscape painting. It is made from Imagination."
        , mime = Image Jpeg
        , fileName = "Cliff_Ocean.jpg"
        , group = Art.Group.painting
        , tags = [ "Ocean", "Krita", "Landscape" ]
        , buyStatus = Sale.boughtFor 75
        , createdAt = mkCreatedAt 2018 1 5
        , resolution = resolution 1600 1200
        }
    , autoCid
        { name = "Abstract #1"
        , description = "My first attempt at an abstract painting."
        , mime = Image Jpeg
        , fileName = "Color_Clouds.jpg"
        , group = Art.Group.painting
        , tags = [ "Clouds", "Procreate", "Abstract" ]
        , buyStatus = Sale.boughtFor 100
        , createdAt = mkCreatedAt 2021 4 23
        , resolution = resolution 4000 6000
        }
    , autoCid
        { name = "Eagle Landing"
        , description = "A landscape made from imagination"
        , mime = Image Jpeg
        , fileName = "Eagle_Landing.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Landscape" ]
        , buyStatus = Sale.boughtFor 75
        , createdAt = mkCreatedAt 2021 3 2
        , resolution = resolution 2732 2048
        }
    , autoCid
        { name = "Fire Woman"
        , description = "A copy of another artist's work. It was made for practice."
        , mime = Image Jpeg
        , fileName = "firewomen.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Portrait", "Character" ]
        , buyStatus = Sale.notForSale
        , createdAt = mkCreatedAt 2020 12 30
        , resolution = resolution 2048 2732
        }
    , selfPortraitTwo
    , autoCid
        { name = "Self Portrait #1"
        , description = "A self portrait."
        , mime = Image Jpeg
        , fileName = "Self_Portrait_1.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Portrait" ]
        , buyStatus = Sale.notForSale
        , createdAt = mkCreatedAt 2020 1 12
        , resolution = resolution 2208 2944
        }
    , autoCid
        { name = "A Frog"
        , description = "A Frog."
        , mime = Image Jpeg
        , fileName = "Frog.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Animal" ]
        , buyStatus = Sale.boughtFor 100
        , createdAt = mkCreatedAt 2019 8 10
        , resolution = resolution 2560 1440
        }
    , autoCid
        { name = "Golden Girl"
        , description = "A copy of another artist's work. It was made for practice."
        , mime = Image Jpeg
        , fileName = "Golden_Girl.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Portrait", "Character" ]
        , buyStatus = Sale.notForSale
        , createdAt = mkCreatedAt 2021 1 15
        , resolution = resolution 2000 3786
        }
    , autoCid
        { name = "Milk Man"
        , description = "A milk man making a goofy face."
        , mime = Image Jpeg
        , fileName = "Milk_Man.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Portrait", "Character" ]
        , buyStatus = Sale.boughtFor 100
        , createdAt = mkCreatedAt 2021 1 28
        , resolution = resolution 3000 4000
        }
    , autoCid
        { name = "Aristocrat"
        , description = "A copy of another artist's work. It was made for practice."
        , mime = Image Jpeg
        , fileName = "Aristocrat-Dishonored.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Portrait", "Character", "Dishonored" ]
        , buyStatus = Sale.notForSale
        , createdAt = mkCreatedAt 2020 7 4
        , resolution = resolution 1700 3000
        }
    , autoCid
        { name = "Priest"
        , description = "A character made from imagination."
        , mime = Image Jpeg
        , fileName = "Priest.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Portrait", "Character" ]
        , buyStatus = Sale.notForSale
        , createdAt = mkCreatedAt 2020 6 30
        , resolution = resolution 1724 2400
        }
    , autoCid
        { name = "Volwerine"
        , description = "A copy of another artist's work. It was made for practice."
        , mime = Image Jpeg
        , fileName = "Volwerine.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Portrait", "Character" ]
        , buyStatus = Sale.notForSale
        , createdAt = mkCreatedAt 2020 8 20
        , resolution = resolution 2556 2820
        }
    , autoCid
        { name = "Pumpkins"
        , description = "A bunch of pumpkins in a field."
        , mime = Image Jpeg
        , fileName = "Pumpkins.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Fruit", "Landscape" ]
        , buyStatus = Sale.boughtFor 100
        , createdAt = mkCreatedAt 2019 8 10
        , resolution = resolution 1800 1200
        }
    , autoCid
        { name = "Purple Flowers In The Jungle"
        , description = "A jungle scene from my imagination."
        , mime = Image Jpeg
        , fileName = "Purple_Flowers_In_Jungle.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Landscape" ]
        , buyStatus = Sale.boughtFor 100
        , createdAt = mkCreatedAt 2019 5 25
        , resolution = resolution 1200 1920
        }
    , autoCid
        { name = "Purple Women"
        , description = "A copy of another artist's work. It was made for practice and was the first portriat I ever did on my ipad."
        , mime = Image Jpeg
        , fileName = "Purple_Woman.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Portrait", "Character", "Dishonored" ]
        , buyStatus = Sale.notForSale
        , createdAt = mkCreatedAt 2020 10 30
        , resolution = resolution 2048 2732
        }
    , autoCid
        { name = "Wave"
        , description = "My first ever digital painting."
        , mime = Image Jpeg
        , fileName = "Wave.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Ocean" ]
        , buyStatus = Sale.boughtFor 50
        , createdAt = mkCreatedAt 2019 12 17
        , resolution = resolution 1800 1200
        }
    , autoCid
        { name = "Whaligoe Steps"
        , description = "Whaligoe Steps in Scotland but with the colors creatively re-imagined."
        , mime = Image Jpeg
        , fileName = "Whaligoe_Steps.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Ocean", "Landscape" ]
        , buyStatus = Sale.boughtFor 150
        , createdAt = mkCreatedAt 2019 8 25
        , resolution = resolution 2560 1440
        }
    , autoCid
        { name = "Where Is My Bull"
        , description = "Where is my bull? Where is he?"
        , mime = Image Jpeg
        , fileName = "Where_Is_My_Bull.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Animal", "Bull" ]
        , buyStatus = Sale.boughtFor 100
        , createdAt = mkCreatedAt 2021 8 2
        , resolution = resolution 6000 4000
        }
    , autoCid
        { name = "Winter Winds"
        , description = "A few trees drowning in a snowstorm."
        , mime = Image Jpeg
        , fileName = "Winter_Winds.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Winter" ]
        , buyStatus = Sale.boughtFor 30
        , createdAt = mkCreatedAt 2018 5 4
        , resolution = resolution 1600 1200
        }
    , autoCid
        { name = "Pomegranate"
        , description = "A delicious pomegranate."
        , mime = Image Jpeg
        , fileName = "pomegranate.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Still Life" ]
        , buyStatus = Sale.boughtFor 75
        , createdAt = mkCreatedAt 2019 10 13
        , resolution = resolution 1920 1440
        }
    , autoCid
        { name = "Boy"
        , description = "A study in grey scale painting."
        , mime = Image Jpeg
        , fileName = "Grey_Boy.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Portrait" ]
        , buyStatus = Sale.boughtFor 50
        , createdAt = mkCreatedAt 2020 5 6
        , resolution = resolution 1600 2140
        }
    , autoCid
        { name = "Magical Rocks"
        , description = "A copy of another artist's work. It was made for practice."
        , mime = Image Jpeg
        , fileName = "Magical_Rocks.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Landscape" ]
        , buyStatus = Sale.notForSale
        , createdAt = mkCreatedAt 2020 8 29
        , resolution = resolution 2256 2820
        }
    , autoCid
        { name = "Self Portrait #0"
        , description = "My first self portait"
        , mime = Image Jpeg
        , fileName = "Self_Portrait_0.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Self Portrait" ]
        , buyStatus = Sale.notForSale
        , createdAt = mkCreatedAt 2019 7 28
        , resolution = resolution 4200 3300
        }
    , autoCid
        { name = "Orange Tree"
        , description = "Found an unfinshed piece that I decided to finish. I had problem converying the perspective but managed to solve it by adding a boat. "
        , mime = Image Jpeg
        , fileName = "Orange_Tree.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Procreate", "Landscape" ]
        , buyStatus = Sale.boughtFor 100
        , createdAt = mkCreatedAt 2021 8 10
        , resolution = resolution 1200 1006
        }
    , autoCid
        { name = "Fluffy Cloud"
        , description = "Another unfinshed piece that painted on top of. Originally made in Krita but then modified in Procreate."
        , mime = Image Jpeg
        , fileName = "fluffy_cloud_2021-08-11.jpg"
        , group = Art.Group.painting
        , tags = [ "Krita", "Procreate", "Landscape", "Cloud" ]
        , buyStatus = Sale.boughtFor 50
        , createdAt = mkCreatedAt 2021 8 11
        , resolution = resolution 1000 1000
        }
    , autoCid
        { name = "Impotence"
        , description = "Another piece in my series of \"famous works but with bananas\". This time it is \"The Persistance of Memory\" by Salvador Dali that gets the banana treatment."
        , mime = Image Jpeg
        , fileName = "Impotence.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Surrealism", "Banana" ]
        , buyStatus = Sale.boughtFor 200
        , createdAt = mkCreatedAt 2021 8 21
        , resolution = resolution 6000 3374
        }
    , autoCid
        { name = "The Young Bull"
        , description = "To celebrate the successfull launch and getting all my NFTs sold I gifted this NFT to one member of the community."
        , mime = Image Jpeg
        , fileName = "The_young_bull.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Bull" ]
        , buyStatus = Sale.Gifted
        , createdAt = mkCreatedAt 2021 9 3
        , resolution = resolution 6000 4000
        }
    , autoCid
        { name = "The Skier"
        , description = "A painting of my dad skiing."
        , mime = Image Jpeg
        , fileName = "Dad_Skiing.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Winter", "Skiing" ]
        , buyStatus = Sale.NotForSale
        , createdAt = mkCreatedAt 2021 6 19
        , resolution = resolution 6000 4000
        }
    , autoCid
        { name = "Farsan"
        , description = "This is a painting of an old photo of my dad who was a TV repair man at the time."
        , mime = Image Jpeg
        , fileName = "Farsan.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Sweden", "Old School Cool" ]
        , buyStatus = Sale.NotForSale
        , createdAt = mkCreatedAt 2021 8 28
        , resolution = resolution 6000 4000
        }
    , autoCid
        { name = "Tiny Tsunami"
        , description = "Yet again I'm taking a old piece of art and adding bananas to it. This time it is 'the great wave off kanagawa' by the Japanese ukiyo-e artist Hokusai."
        , mime = Image Jpeg
        , fileName = "Tiny_Tsunami.jpg"
        , group = Art.Group.painting
        , tags = [ "Procreate", "Banana", "Japan" ]
        , buyStatus = Sale.NotForSale
        , createdAt = mkCreatedAt 2021 9 17
        , resolution = resolution 3859 2594
        }
    ]


motion : List Doc
motion =
    let
        notSold =
            Set.fromList [ "124", "120", "199", "109", "108", "107", "100", "099", "098", "097", "094", "077", "076", "074", "064", "062", "061", "060", "056", "052", "051", "050", "049", "046", "044", "042", "039", "037", "036", "042", "033", "031", "028", "027", "021", "020", "013", "010", "006", "004" ]

        wasBought : String -> Bool
        wasBought name =
            not <| Set.member name notSold

        template name =
            { cid = "MOTION-" ++ name
            , name = String.padLeft 3 '0' name
            , description =
                [ Element.paragraph
                    []
                    [ Element.el [ Font.bold ] <| Element.text "MOTION"
                    , Element.text " is a series of 128 unique generative artworks."
                    , Element.text " Each artwork can be bought as a one-of-a-kind NFT and includes:"
                    , Element.text " a video thumbnail (displayed in NFT marketplaces),"
                    , Element.text " a 30s video optimized for social media,"
                    , Element.text " the code and data to generate longer/higher quality videos."
                    ]
                , Element.paragraph
                    []
                    [ Element.text " To create this series I thought of the program I wrote more of as a tool then an independent process for generating art."
                    , Element.text " In my last work (AlgoMarble) there was no human intervention;"
                    , Element.text " It simply generated what it generated."
                    , Element.text " This time I used my program more like a abstract brush and used it to come up 128 unqiue creations."
                    ]
                , Element.paragraph
                    []
                    [ Element.text " I had seen people use vector fields (or flow fields) to create very dynamic images so I thought that maybe I could place particles inside of them and let them move around."
                    , Element.text " Of course I'm not the first person to explore this idea;"
                    , Element.text " there is an excellent webapp developed by Andrei Kashcha called field play (search for it on google) that I played around with for a bit for getting inspiration."
                    , Element.text " If you want to checkout the code I wrote you can do so at the "
                    , Element.link
                        [ Font.color DesignSystem.Theme.defaultTheme.colorScheme.primaryBrand.color
                        ]
                        { url = "https://gitlab.com/HampusWeslien/motion"
                        , label = Element.text "git repo"
                        }
                    , Element.text ". Where you will also find further explanations and instructions for generating high quality videos."
                    ]
                , Element.paragraph
                    []
                    [ Element.text "I'm trying to experiment a bit with monitization of my work."
                    , Element.text " Note that all NFTs in the MOTION collection have 2.5% royalty when sold through a marketplace that supports the CIP-027 standard."
                    , Element.text " What does this mean for you as a buyer?"
                    , Element.text " It means that if you resell yout NFT for 100 ADA then 2.5 ADA will be sent to me."
                    ]
                ]
            , group = Art.Group.Motion
            , shortDescription = "MOTION nbr " ++ name
            , mime = Video MP4
            , fileName = "MOTION-" ++ name ++ ".png"
            , tags = [ "Generative", "MOTION" ]
            , buyStatus =
                if wasBought name then
                    Sale.boughtFor 35

                else
                    Sale.notForSale
            , createdAt = mkCreatedAt 2021 10 27
            , resolution = resolution 1920 1080
            }
    in
    List.map (template << String.fromInt) <| List.range 1 128


algoMarble : List Doc
algoMarble =
    let
        notSold =
            Set.fromList [ "457", "448", "437", "431", "418", "416", "415", "395", "376", "367", "354", "323", "306", "302", "282", "271", "270", "259", "236", "162", "152", "127", "109", "103" ]

        wasBought : String -> Bool
        wasBought name =
            not <| Set.member name notSold

        template name =
            { cid = "AlgoMarble-" ++ name
            , name = String.padLeft 4 '0' name
            , description =
                [ Element.paragraph
                    []
                    [ Element.el [ Font.bold ] <| Element.text "AlgoMarble"
                    , Element.text " is a series of 512 unique generative artworks."
                    , Element.text " Each artwork can be bought as a one-of-a-kind NFT."
                    ]
                , Element.paragraph
                    []
                    [ Element.text "The basic idea was to stack layers of noise on top of each other until fantastical textures emerged."
                    , Element.text " This is not a completely new idea but can trace its orgin back as far as 1983 with the invention of Perlin noise by Ken Perlin."
                    , Element.text " If you want to check out the code you can head over to the "
                    , Element.link
                        [ Font.color DesignSystem.Theme.defaultTheme.colorScheme.primaryBrand.color
                        ]
                        { url = "https://gitlab.com/HampusWeslien/algomarble"
                        , label = Element.text "git repo"
                        }
                    , Element.text ". Where you will also find further explanations and instructions for generating super high resolution images."
                    , Element.text <| " The algorithm uses the name of the artwork as the seed, in this case the seed is '" ++ name ++ "'."
                    ]
                , Element.paragraph
                    []
                    [ Element.text "There is also a "
                    , Element.link
                        [ Font.color DesignSystem.Theme.defaultTheme.colorScheme.primaryBrand.color
                        ]
                        { url = "https://youtu.be/q1AVe5wOdR4"
                        , label = Element.text "youtube video"
                        }
                    , Element.text " that explain the basic idea behind the algorithm."
                    ]
                , Element.paragraph
                    []
                    [ Element.text "Some of the images are not made justice when they are made to small."
                    , Element.text " If the image looks pixelated I recommend downloading the high resolution image; you find the download link beneath the image."
                    ]
                ]
            , group = Art.Group.AlgoMarble
            , shortDescription = "AlgoMarble with seed " ++ name
            , mime = Image Png
            , fileName = name ++ ".png"
            , tags = [ "Generative", "AlgoMarble" ]
            , buyStatus =
                if wasBought name then
                    Sale.boughtFor 25

                else
                    Sale.notForSale
            , createdAt = mkCreatedAt 2021 9 8
            , resolution = resolution 2400 1600
            }
    in
    List.map (template << String.fromInt) <| List.range 0 511


stainedGlass : List Doc
stainedGlass =
    let
        nameToGroup : String -> Group
        nameToGroup name =
            case name of
                "93700844" ->
                    Art.Group.stainedGlass "Genesis"

                "73521339" ->
                    Art.Group.stainedGlass "Dawn of Light"

                "91167350" ->
                    Art.Group.stainedGlass "King of Arkham"

                "98763993" ->
                    Art.Group.stainedGlass "Tangerine Dream"

                "88050797" ->
                    Art.Group.stainedGlass "Vaccine"

                "50174834" ->
                    Art.Group.stainedGlass "Emerald"

                "78653823" ->
                    Art.Group.stainedGlass "Crystal Meth"

                "97309529" ->
                    Art.Group.stainedGlass "stained_unsig00661"

                "89813951" ->
                    Art.Group.stainedGlass "Wings"

                "20063884" ->
                    Art.Group.stainedGlass "Eggs over Mauve"

                "28684357" ->
                    Art.Group.stainedGlass "Lavender Haze"

                "81975302" ->
                    Art.Group.stainedGlass "Strat Blue Burst"

                "24489465" ->
                    Art.Group.stainedGlass "The Big Picture"

                "52415956" ->
                    Art.Group.stainedGlass "Turkish Delight"

                _ ->
                    Art.Group.stainedGlassNoName

        template fileName =
            let
                name =
                    String.slice 0 8 fileName
            in
            { cid = "Stained-Glass-" ++ name
            , name = name
            , description =
                [ Element.paragraph
                    []
                    [ Element.el [ Font.bold ] <| Element.text "Stained Glass"
                    , Element.text " is a series of 100 unique generative artworks."
                    , Element.text " Each artwork can be bought as a one-of-a-kind NFT."
                    ]
                , Element.paragraph
                    []
                    [ Element.text "A few of the artworks have also been given a community name (within parentheses) by the first owner. "
                    , Element.text "If a community name hasn't already been given, and you own the piece, you can "
                    , Element.link
                        [ Font.color DesignSystem.Theme.defaultTheme.colorScheme.primaryBrand.color
                        ]
                        { url = "https://docs.google.com/forms/d/e/1FAIpQLSej_g9qwhRltYhTmyp3oAJeCTeusplBQgy5Ft8dMO4whEt70w/viewform?usp=sf_link"
                        , label = Element.text "fill this form"
                        }
                    , Element.text " and DM me on twitter (so I get notified). "
                    , Element.text "Two rules: no obscenities, and it must be in English."
                    ]
                , Element.paragraph
                    []
                    [ Element.text " They were created by splitting triangles recursively"
                    , Element.text ", each with its own unique combination of how often a split occurs, how the triangles are split, and the color palette."
                    , Element.text " The images are stored as SVG images (a.k.a. infinite resolution). They were designed to work as background images on any screen: mobile, tablet, or desktop."
                    ]
                , Element.paragraph
                    []
                    [ Element.text " If you want to take an indepth look at the code check out the "
                    , Element.link
                        [ Font.color DesignSystem.Theme.defaultTheme.colorScheme.primaryBrand.color
                        ]
                        { url = "https://gitlab.com/HampusWeslien/stained-glass"
                        , label = Element.text "git repo"
                        }
                    , Element.text "."
                    , Element.text <| " The number/name of the image (" ++ name ++ ") is used as the seed."
                    ]
                ]
            , group = nameToGroup name
            , shortDescription = "Stained Glass with seed " ++ name
            , mime = Image Svg
            , fileName = fileName
            , tags = [ "Generative", "Stained Glass", "Triangles" ]
            , buyStatus = Sale.boughtFor 25
            , createdAt = mkCreatedAt 2021 8 13
            , resolution = resolution 1200 1200
            }
    in
    List.map template
        [ "00456830.svg"
        , "00534964.svg"
        , "03851774.svg"
        , "07532852.svg"
        , "08247708.svg"
        , "08294677.svg"
        , "08523360.svg"
        , "09453704.svg"
        , "09901327.svg"
        , "10480308.svg"
        , "12333805.svg"
        , "13582759.svg"
        , "14166929.svg"
        , "14381240.svg"
        , "15315881.svg"
        , "15925273.svg"
        , "16158626.svg"
        , "16720912.svg"
        , "16882072.svg"
        , "17721837.svg"
        , "19082677.svg"
        , "19137925.svg"
        , "19762503.svg"
        , "20063884.svg"
        , "20303503.svg"
        , "22764432.svg"
        , "24489465.svg"
        , "26909933.svg"
        , "28684357.svg"
        , "29881793.svg"
        , "31076848.svg"
        , "32756390.svg"
        , "34648215.svg"
        , "36738089.svg"
        , "39299120.svg"
        , "39539625.svg"
        , "40818528.svg"
        , "40915924.svg"
        , "43718143.svg"
        , "44139434.svg"
        , "44266196.svg"
        , "44315178.svg"
        , "45099050.svg"
        , "45148072.svg"
        , "45706685.svg"
        , "46521515.svg"
        , "48842927.svg"
        , "49286484.svg"
        , "49515637.svg"
        , "50174834.svg"
        , "51959128.svg"
        , "52098493.svg"
        , "52162153.svg"
        , "52332483.svg"
        , "52415956.svg"
        , "54986188.svg"
        , "56061724.svg"
        , "57498966.svg"
        , "63808790.svg"
        , "64189713.svg"
        , "64196293.svg"
        , "64974370.svg"
        , "68652572.svg"
        , "71169845.svg"
        , "73485237.svg"
        , "73521339.svg"
        , "74257847.svg"
        , "74284065.svg"
        , "75791387.svg"
        , "78653823.svg"
        , "79656571.svg"
        , "80403573.svg"
        , "81042545.svg"
        , "81376648.svg"
        , "81975302.svg"
        , "82735458.svg"
        , "83076266.svg"
        , "83347279.svg"
        , "83420429.svg"
        , "85843737.svg"
        , "86963292.svg"
        , "87394256.svg"
        , "87759780.svg"
        , "88050797.svg"
        , "88111512.svg"
        , "89813951.svg"
        , "90533424.svg"
        , "91167350.svg"
        , "93160699.svg"
        , "93700844.svg"
        , "94463622.svg"
        , "95213307.svg"
        , "95783722.svg"
        , "96297881.svg"
        , "97047055.svg"
        , "97309529.svg"
        , "97932002.svg"
        , "98212139.svg"
        , "98763993.svg"
        , "99270561.svg"
        ]
