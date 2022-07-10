module Art.Group exposing
    ( Group(..)
    , allGroups
    , fuzz
    , isAlgoMarble
    , isMotion
    , isPainting
    , isStainedGlass
    , painting
    , path
    , policyIds
    , sameGroup
    , stainedGlass
    , stainedGlassNoName
    , toString
    )

import Fuzz


{-| Each artwork is part of a group.


# Group

@docs Group, allGroups


# Constructors

@docs painting, stainedGlassNoName, stainedGlass


# Boolean

@docs sameGroup, isStainedGlass, isAlgoMarble, isMotion, isPainting


# Utility

@docs toString, path, nftMakerProjectID, policyIds

#Fuzz

@docs fuzz

-}
type Group
    = Painting
    | StainedGlass { communityName : Maybe String }
    | AlgoMarble
    | Motion


{-| A lot of code depends on known all possible groups. This makes it easy to update all does
places at once.
-}
allGroups : List Group
allGroups =
    [ Painting, stainedGlassNoName, AlgoMarble, Motion ]



--------------------------------------------------------------------------------
-- Boolean Checks
--------------------------------------------------------------------------------


{-| Check if the group is StainedGlass
-}
isStainedGlass : Group -> Bool
isStainedGlass =
    sameGroup stainedGlassNoName


{-| Check if the group is AlgoMarble
-}
isAlgoMarble : Group -> Bool
isAlgoMarble =
    sameGroup AlgoMarble


{-| Check if the group is Paiting
-}
isPainting : Group -> Bool
isPainting =
    sameGroup Painting


{-| Check if the group is Motion
-}
isMotion : Group -> Bool
isMotion =
    sameGroup Motion


{-| Check that two groups are of the same base type.
-}
sameGroup : Group -> Group -> Bool
sameGroup g1 g2 =
    case ( g1, g2 ) of
        ( Painting, Painting ) ->
            True

        ( StainedGlass _, StainedGlass _ ) ->
            True

        ( AlgoMarble, AlgoMarble ) ->
            True

        ( Motion, Motion ) ->
            True

        ( _, _ ) ->
            False



--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------


{-| Smart constructor for the painting group
-}
painting : Group
painting =
    Painting


{-| Smart constructor for the stained glass group
-}
stainedGlassNoName : Group
stainedGlassNoName =
    StainedGlass { communityName = Nothing }



{- constructor

   The community name was choosen but the buyers from the original sale :)
   Here is the form
   https://docs.google.com/spreadsheets/d/1w31bp6cu4QnouOIbDZ4nuw5CvikUM1kuwfk8_Va6tLI/edit?resourcekey#gid=984077906

-}


stainedGlass : String -> Group
stainedGlass s =
    StainedGlass { communityName = Just s }



--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------


{-| Turn the Group into a String
-}
toString : Group -> String
toString group =
    case group of
        Painting ->
            "Painting"

        StainedGlass _ ->
            "Stained Glass"

        AlgoMarble ->
            "AlgoMarble"

        Motion ->
            "MOTION"


{-| Get the path where the Groups images are stored
-}
path : Group -> String
path group =
    case group of
        Painting ->
            "/assets/art/painting/"

        StainedGlass _ ->
            "/assets/art/stained-glass/"

        AlgoMarble ->
            "/assets/art/algo-marble/"

        Motion ->
            "/assets/art/motion/"


{-| Returns the policy ids associated with the nfts
-}
policyIds : Group -> List String
policyIds group =
    case group of
        Painting ->
            [ "dd04ad427b8c2f76409502907063239518d81ad7415046e170d3da07", "6ce34ca46c79a999643e9d58f04feca4e609370ca8745344dd08b443" ]

        StainedGlass _ ->
            [ "8236e14a07850dec626f69977da8700297a575ded7c5f26c957be731" ]

        AlgoMarble ->
            [ "fc7f00513d26a5c4de57a5863f0849559493d5ec008951eeb65ed3f1" ]

        Motion ->
            [ "27706b242382999c8cb7e79c4bc7171a37a57768cf8f4b44754d3906" ]



--------------------------------------------------------------------------------
-- Fuzzer
--------------------------------------------------------------------------------


fuzz : Fuzz.Fuzzer Group
fuzz =
    Fuzz.oneOf
        [ Fuzz.constant Painting
        , Fuzz.constant stainedGlassNoName
        , Fuzz.map stainedGlass <| Fuzz.string
        , Fuzz.constant AlgoMarble
        , Fuzz.constant Motion
        ]
