module Route exposing
    ( Route(..), initArt, fromUrl, replaceUrl, routeToPath, routeToString
    , ArtQuery, paginateBackward, paginateForward, setGroup, setForSale, setSearch
    , fuzz
    )

{-| This module is reposonible for defining and parsing all routes used through
out the app.


# Route

@docs Route, initArt, fromUrl, replaceUrl, routeToPath, routeToString


# Query

@docs ArtQuery, paginateBackward, paginateForward, setGroup, setForSale, setSearch


# Fuzzer

@docs fuzz

-}

import Art.Group
import Browser.Navigation as Nav
import Components.Pagination as Pagination
import Dict
import Fuzz
import Helpers exposing (uncurry)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query



--------------------------------------------------------------------------------
-- Routing
--------------------------------------------------------------------------------


type alias ArtQuery =
    { forSale : Bool
    , group : Maybe Art.Group.Group
    , search : String
    , pageSize : Int
    , page : Int
    }


artQueryFuzz : Fuzz.Fuzzer ArtQuery
artQueryFuzz =
    let
        validInt =
            Fuzz.intRange 1 10000

        validGroup =
            Fuzz.oneOf
                [ Fuzz.constant Art.Group.stainedGlassNoName
                , Fuzz.constant Art.Group.painting
                , Fuzz.constant Art.Group.AlgoMarble
                ]
    in
    Fuzz.map5 ArtQuery Fuzz.bool (Fuzz.maybe validGroup) Fuzz.string validInt validInt


{-| Paginate forward. Needs to know the total number of items it is paginating over.
-}
paginateForward : Int -> ArtQuery -> Maybe ArtQuery
paginateForward length query =
    Pagination.paginateForward { pageSize = query.pageSize, page = query.page, end = length }
        |> Maybe.map (\newPage -> { query | page = newPage })


{-| Paginate backward. Needs to know the total number of items it is paginating over.
-}
paginateBackward : Int -> ArtQuery -> Maybe ArtQuery
paginateBackward length query =
    Pagination.paginateBackward { pageSize = query.pageSize, page = query.page, end = length }
        |> Maybe.map (\newPage -> { query | page = newPage })


initArtQuery : ArtQuery
initArtQuery =
    { forSale = False
    , group = Nothing
    , search = ""
    , pageSize = 24
    , page = 0
    }


{-| The possible routes this web app supports. Any other route will end
you on the 404 page.
-}
type Route
    = Art ArtQuery
    | FAQ
    | Me
    | Home
    | Artwork String


fuzz : Fuzz.Fuzzer Route
fuzz =
    Fuzz.oneOf
        [ Fuzz.map Art artQueryFuzz
        , Fuzz.constant FAQ
        , Fuzz.constant Me
        , Fuzz.constant Home
        , Fuzz.map (Artwork << String.fromInt) Fuzz.int
        ]


initArt : Route
initArt =
    Art initArtQuery


parser : Parser.Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Home (Parser.s "home")
        , Parser.map Art (Parser.s "art" <?> artQueryParser)
        , Parser.map FAQ (Parser.s "faq")
        , Parser.map Me (Parser.s "me")
        , Parser.map Artwork (Parser.s "artwork" </> Parser.string)
        ]


artQueryParser : Query.Parser ArtQuery
artQueryParser =
    let
        forSale : Query.Parser (Maybe Bool)
        forSale =
            [ ( "true", True ), ( "false", False ), ( "True", True ), ( "False", False ) ]
                |> Dict.fromList
                |> Query.enum "forSale"

        search : Query.Parser (Maybe String)
        search =
            Query.string "search"

        pageSize : Query.Parser (Maybe Int)
        pageSize =
            Query.int "pageSize"

        page : Query.Parser (Maybe Int)
        page =
            Query.int "page"

        group : Query.Parser (Maybe Art.Group.Group)
        group =
            Art.Group.allGroups
                |> List.map (\g -> ( Art.Group.toString g, g ))
                |> Dict.fromList
                |> Query.enum "group"

        combineResult mForsale mGroup mSearch mPageSize mPage =
            { forSale = Maybe.withDefault False mForsale
            , group = mGroup
            , search = Maybe.withDefault "" mSearch
            , pageSize = Maybe.withDefault 24 mPageSize
            , page = Maybe.withDefault 0 mPage
            }
    in
    Query.map5 combineResult forSale group search pageSize page


{-| Correctly handles when the user updates the group
-}
setGroup : ArtQuery -> Maybe Art.Group.Group -> ArtQuery
setGroup query newGroup =
    if query.group == newGroup then
        query

    else
        { query | group = newGroup, page = 0 }


{-| Correctly handles when the user updates the forSale
-}
setForSale : ArtQuery -> Bool -> ArtQuery
setForSale query newForSale =
    if query.forSale == newForSale then
        query

    else
        { query | forSale = newForSale, page = 0 }


{-| Correctly handles when the user updates the search text
-}
setSearch : ArtQuery -> String -> ArtQuery
setSearch query newSearch =
    if query.search == newSearch then
        query

    else
        { query | search = newSearch, page = 0 }



--------------------------------------------------------------------------------
-- Public Helpers
--------------------------------------------------------------------------------


routeToString : Route -> String
routeToString route =
    case route of
        Art _ ->
            "Art"

        Home ->
            "Home"

        FAQ ->
            "FAQ"

        Me ->
            "Me"

        -- Those bellow here are never used in practice
        Artwork id ->
            "Artwork " ++ id


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToPath route)


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser



--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------


routeToPath : Route -> String
routeToPath =
    uncurry Builder.absolute << routeToPieces


routeToPieces : Route -> ( List String, List Builder.QueryParameter )
routeToPieces route =
    case route of
        Art artQuery ->
            ( [ "art" ], toArtQueryParam artQuery )

        FAQ ->
            ( [ "faq" ], [] )

        Home ->
            ( [ "home" ], [] )

        Me ->
            ( [ "me" ], [] )

        Artwork id ->
            ( [ "artwork", id ], [] )


toArtQueryParam : ArtQuery -> List Builder.QueryParameter
toArtQueryParam artQuery =
    [ Builder.string "search" artQuery.search
    , Builder.string "forSale" <| Helpers.fromBool artQuery.forSale
    , Builder.int "pageSize" <| artQuery.pageSize
    , Builder.int "page" <| artQuery.page
    ]
        |> (case artQuery.group of
                Just group ->
                    (::) (Builder.string "group" <| Art.Group.toString group)

                _ ->
                    identity
           )
