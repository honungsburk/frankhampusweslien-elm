module Page.Art exposing (Model, Msg, init, mapSession, toSession, update, view)

import Art.Doc exposing (Doc, SaleStateCounts)
import Art.Group
import Art.Sale as Sale
import Browser.Navigation as Nav
import Cardano.Ada as Ada
import Components.Hairline exposing (hairline)
import Components.Pagination as Pagination
import DesignSystem exposing (DesignSystem)
import DesignSystem.Fonts exposing (roboto)
import DesignSystem.Responsiveness
import DesignSystem.Theme exposing (ColorPair, Theme)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Extra exposing (onEnter)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import ElmTextSearch
import Helpers exposing (segmentEvery)
import Route
import Session exposing (Session)



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { session : Session
    , lookupArtDocs : Dict String Doc
    , index : ElmTextSearch.Index Doc
    , artToShow : Result String ( List Doc, Pagination.PaginationInfo )
    , searchText : String
    , nftCounts : SaleStateCounts
    , query : Route.ArtQuery
    }


init : Session -> Route.ArtQuery -> ( Model, Cmd Msg )
init session query =
    let
        lookupArtDocs =
            Art.Doc.makeLookupIndex <| session.artDocs

        initNFTCountsBar =
            Art.Doc.countSaleStates session.artDocs

        res =
            doSearch
                { query = query
                , index = Tuple.first Art.Doc.artIndex
                , lookupArtDocs = lookupArtDocs
                }
                session.artDocs

        initModel =
            case res of
                Err msg ->
                    { session = session
                    , lookupArtDocs = lookupArtDocs
                    , index = Tuple.first Art.Doc.artIndex
                    , artToShow = Err msg
                    , searchText = query.search
                    , nftCounts = initNFTCountsBar
                    , query = query
                    }

                Ok ( newIndex, page, pageInfo ) ->
                    { session = session
                    , lookupArtDocs = lookupArtDocs
                    , index = newIndex
                    , artToShow = Ok ( page, pageInfo )
                    , searchText = query.search
                    , nftCounts = initNFTCountsBar
                    , query = query
                    }
    in
    ( initModel
    , Cmd.none
    )



--------------------------------------------------------------------------------
-- Query
--------------------------------------------------------------------------------


doSearch :
    { query : Route.ArtQuery
    , index : ElmTextSearch.Index Doc
    , lookupArtDocs : Dict String Doc
    }
    -> List Doc
    -> Result String ( ElmTextSearch.Index Doc, List Doc, Pagination.PaginationInfo )
doSearch settings docs =
    (if settings.query.search == "" then
        Ok ( settings.index, docs )

     else
        ElmTextSearch.search settings.query.search settings.index
            |> Result.map (Tuple.mapSecond (List.filterMap (\( v, _ ) -> Dict.get v settings.lookupArtDocs)))
    )
        |> Result.map (Tuple.mapSecond (applyFilters settings.query.forSale settings.query.group))
        |> Result.map
            (\( index, foundDocs ) ->
                let
                    ( pagedDocs, pageInfo ) =
                        Pagination.paginate
                            { pageSize = settings.query.pageSize
                            , page = settings.query.page
                            }
                            foundDocs
                in
                ( index, pagedDocs, pageInfo )
            )


applyFilters : Bool -> Maybe Art.Group.Group -> List Doc -> List Doc
applyFilters filterForPurchase activeGroups =
    let
        filterOne =
            if filterForPurchase then
                Art.Doc.forSale

            else
                always True

        filterTwo =
            case activeGroups of
                Nothing ->
                    always True

                Just group ->
                    Art.Doc.hasGroup group
    in
    List.filter (\doc -> filterTwo doc && filterOne doc)



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = UpdateSearchBar String
    | Search String
    | FlipPurchaseFilter
    | FilterGroup Art.Group.Group
    | PaginateForwards
    | PaginateBackwards


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSearchBar text ->
            ( { model | searchText = text }, Cmd.none )

        Search text ->
            let
                query =
                    model.query

                newQuery =
                    Route.setSearch query text
            in
            applyQuery model newQuery

        FlipPurchaseFilter ->
            let
                query =
                    model.query

                newQuery =
                    Route.setForSale query <| not query.forSale
            in
            applyQuery model newQuery

        FilterGroup group ->
            let
                newActiveGroup =
                    case model.query.group of
                        Just otherGroup ->
                            if group == otherGroup then
                                Nothing

                            else
                                Just group

                        Nothing ->
                            Just group

                query =
                    model.query

                newQuery =
                    Route.setGroup query newActiveGroup
            in
            applyQuery model newQuery

        PaginateForwards ->
            case model.artToShow of
                Err _ ->
                    ( model, Cmd.none )

                Ok ( _, pageInfo ) ->
                    Route.paginateForward pageInfo.total model.query
                        |> Maybe.map (applyQuery model)
                        |> Maybe.withDefault ( model, Cmd.none )

        PaginateBackwards ->
            case model.artToShow of
                Err _ ->
                    ( model, Cmd.none )

                Ok ( _, pageInfo ) ->
                    Route.paginateBackward pageInfo.total model.query
                        |> Maybe.map (applyQuery model)
                        |> Maybe.withDefault ( model, Cmd.none )


applyQuery : Model -> Route.ArtQuery -> ( Model, Cmd msg )
applyQuery model newQuery =
    case doSearch { query = newQuery, index = model.index, lookupArtDocs = model.lookupArtDocs } model.session.artDocs of
        Err errMsg ->
            ( { model | query = newQuery, artToShow = Err errMsg }, Cmd.none )

        Ok ( newIndex, artToShow, pageInfo ) ->
            ( { model | index = newIndex, query = newQuery, artToShow = Ok ( artToShow, pageInfo ) }
            , Nav.replaceUrl model.session.navKey <| Route.routeToPath (Route.Art newQuery)
            )



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> { title : String, content : Element Msg }
view model =
    { title = "Art"
    , content = Lazy.lazy mainView model
    }


mainView : Model -> Element Msg
mainView model =
    let
        designSystem =
            model.session.designSystem

        pageinationView pInfo =
            Pagination.view
                designSystem.theme
                { info = pInfo
                , goBack = PaginateBackwards
                , goForward = PaginateForwards
                }
    in
    Element.column
        [ Element.padding designSystem.grid.paddingX
        , Element.spacing << Basics.round <| designSystem.theme.spacingRythm 3
        , Element.width Element.fill
        ]
        [ searchArea model
        , case model.artToShow of
            Ok ( page, pageInfo ) ->
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing << round <| designSystem.theme.spacingRythm 2
                    ]
                    [ pageinationView pageInfo
                    , artGrid designSystem page
                    , pageinationView pageInfo
                    ]

            Err errMsg ->
                searchError designSystem.theme errMsg
        ]



--------------------------------------------------------------------------------
-- Search
--------------------------------------------------------------------------------


searchArea : Model -> Element Msg
searchArea model =
    let
        designSystem =
            model.session.designSystem

        isMobile =
            designSystem.device.class == DesignSystem.Responsiveness.Phone
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing
            << Basics.round
            << designSystem.theme.spacingRythm
          <|
            if isMobile then
                3

            else
                2
        ]
        [ Element.el
            [ Element.width << Element.px <| designSystem.widthOfNCol 4
            , Element.centerX
            ]
          <|
            searchbar designSystem.theme model.searchText
        , Element.el [ Element.centerX ] <| nftCountsBar model.session.designSystem model.nftCounts
        , filterBar model
        , hairline designSystem.theme 4
        ]


searchbar : Theme -> String -> Element Msg
searchbar theme text =
    Input.search
        [ Border.rounded 16
        , Background.color theme.colorScheme.accent
        , Border.width 0
        , Element.centerY
        , onEnter <| Search text
        ]
        { onChange = UpdateSearchBar
        , text = text
        , placeholder =
            Just
                << Input.placeholder []
            <|
                Element.text "Search"
        , label = Input.labelHidden "Searchbar"
        }



--------------------------------------------------------------------------------
-- Search Error
--------------------------------------------------------------------------------


searchError : Theme -> String -> Element msg
searchError theme =
    Element.el
        [ Font.size << Basics.round <| theme.fontRythm 3
        , Font.bold
        , Font.center
        , Font.family
            [ Font.typeface "Roboto"
            ]
        ]
        << Element.text



--------------------------------------------------------------------------------
-- FilterBar
--------------------------------------------------------------------------------


filterBar : Model -> Element Msg
filterBar model =
    let
        theme =
            model.session.designSystem.theme

        xandy =
            Basics.round <| theme.spacingRythm 1

        hasStainedGlass =
            Maybe.withDefault False <| Maybe.map Art.Group.isStainedGlass model.query.group

        hasAlgoMarble =
            Maybe.withDefault False <| Maybe.map Art.Group.isAlgoMarble model.query.group

        hasPainting =
            Maybe.withDefault False <| Maybe.map Art.Group.isPainting model.query.group

        hasMotion =
            Maybe.withDefault False <| Maybe.map Art.Group.isMotion model.query.group
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing << Basics.round <| theme.spacingRythm 2
        ]
        [ Element.wrappedRow
            [ Element.alignLeft
            , Element.width Element.fill
            , Element.spacingXY xandy xandy
            ]
            [ groupFilterButton theme hasStainedGlass Art.Group.stainedGlassNoName
            , groupFilterButton theme hasAlgoMarble Art.Group.AlgoMarble
            , groupFilterButton theme hasPainting Art.Group.Painting
            , groupFilterButton theme hasMotion Art.Group.Motion
            ]
        , Element.el [ Element.alignRight ] <| purchaseFilterButton model
        ]



--------------------------------------------------------------------------------
-- Purchase Filter
--------------------------------------------------------------------------------


purchaseFilterButton : Model -> Element Msg
purchaseFilterButton model =
    let
        theme =
            model.session.designSystem.theme
    in
    Input.button
        [ Border.rounded 16
        , Element.paddingXY
            (Basics.round <| theme.spacingRythm 2)
            (Basics.round <| theme.spacingRythm 0)
        , if not model.query.forSale then
            Background.color theme.colorScheme.accent

          else
            Background.gradient theme.colorScheme.gradient
        ]
        { onPress = Just FlipPurchaseFilter
        , label =
            Element.el
                [ Font.family
                    [ Font.typeface "Roboto"
                    ]
                , if not model.query.forSale then
                    Font.color theme.colorScheme.black

                  else
                    Font.color theme.colorScheme.white
                ]
            <|
                Element.text "For Sale"
        }



--------------------------------------------------------------------------------
-- Group Filter
--------------------------------------------------------------------------------


groupFilterButton : Theme -> Bool -> Art.Group.Group -> Element Msg
groupFilterButton theme isActive group =
    Input.button
        [ Border.rounded 16
        , Element.paddingXY
            (Basics.round <| theme.spacingRythm 2)
            (Basics.round <| theme.spacingRythm 0)
        , if isActive then
            Background.color theme.colorScheme.dark.color

          else
            Background.color theme.colorScheme.accent
        ]
        { onPress = Just <| FilterGroup group
        , label =
            Element.el
                [ Font.family
                    [ Font.typeface "Roboto"
                    ]
                , if isActive then
                    Font.color theme.colorScheme.dark.text

                  else
                    Font.color theme.colorScheme.black
                ]
                << Element.text
            <|
                Art.Group.toString group
        }



--------------------------------------------------------------------------------
-- Art Grid
--------------------------------------------------------------------------------


artGrid : DesignSystem -> List Doc -> Element msg
artGrid designSystem arts =
    let
        nbrCols =
            designSystem.grid.totalColumns // 2
    in
    Element.column
        [ Element.spacing designSystem.grid.spacingY
        , Element.width Element.fill
        ]
    <|
        (List.map (Lazy.lazy2 artRow designSystem) <| segmentEvery nbrCols arts)


artRow : DesignSystem -> List Doc -> Element msg
artRow designSystem arts =
    Keyed.row
        [ Element.width Element.fill
        , Element.spacing designSystem.grid.spacingX
        ]
    <|
        List.map (\doc -> ( doc.cid, Lazy.lazy2 card designSystem doc )) arts


card : DesignSystem -> Doc -> Element msg
card designSystem art =
    Element.column
        [ Element.width << Element.px <| designSystem.widthOfNCol 2
        , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 16, bottomRight = 16 }
        , Border.shadow
            { offset = ( 0, 0 )
            , size = 0
            , blur = 16
            , color = Element.rgba255 0 0 0 0.2
            }
        ]
        [ Element.link
            []
            { url = Route.routeToPath <| Route.Artwork art.cid
            , label = cardImage designSystem art
            }
        , Element.column
            [ Element.padding << Basics.round <| designSystem.theme.spacingRythm 2
            , Element.spacing << Basics.round <| designSystem.theme.spacingRythm 1
            ]
            [ cardTitle designSystem.theme <| Art.Doc.displayName art
            , cardTags designSystem.theme art.tags
            ]
        ]


cardImage : DesignSystem -> Doc -> Element msg
cardImage designSystem art =
    Element.image
        [ Element.width << Element.px <| designSystem.widthOfNCol 2
        , Element.height << Element.px <| designSystem.widthOfNCol 2
        , Background.color designSystem.theme.colorScheme.accent
        , Element.inFront <| nftStatus designSystem.theme art.buyStatus
        ]
    <|
        { src = Maybe.withDefault "lol" <| Art.Doc.thumbNailSrc art -- TODO: better error reporting
        , description = art.shortDescription
        }


nftStatus : Theme -> Sale.Status -> Element msg
nftStatus theme status =
    let
        nftStateColors =
            theme.colorScheme.nftState

        statusPill : ColorPair -> String -> Element msg
        statusPill colorPair =
            Element.el [ Element.padding (round <| theme.spacingRythm 1), Element.alignRight ]
                << Element.el
                    [ Background.color colorPair.color
                    , Font.color colorPair.text
                    , Border.rounded 16
                    , roboto
                    , Font.size << round <| theme.fontRythm -1
                    , Element.paddingXY (round <| theme.spacingRythm 1) (round <| theme.spacingRythm 0)
                    , Border.color colorPair.text

                    --, Border.width 2
                    ]
                << Element.text
    in
    case status of
        Sale.Gifted ->
            statusPill nftStateColors.other "Gifted"

        Sale.Bought mPrice ->
            statusPill nftStateColors.sold <| Maybe.withDefault "Sold!" <| Maybe.map Ada.priceString mPrice

        Sale.DMIfInterested ->
            statusPill nftStateColors.available "DM Me"

        Sale.NotForSale ->
            statusPill nftStateColors.other "Not For Sale"


cardTitle : Theme -> String -> Element msg
cardTitle theme t =
    Element.paragraph
        [ Font.size << Basics.round <| theme.fontRythm 0
        , Font.regular
        , Font.family
            [ Font.typeface "Roboto"
            ]
        ]
        [ Element.text t ]


cardTags : Theme -> List String -> Element msg
cardTags theme =
    Element.wrappedRow
        [ Element.spacing << Basics.round <| theme.spacingRythm 0
        ]
        << List.map (cardTag theme)
        << List.take 5


cardTag : Theme -> String -> Element msg
cardTag theme =
    Element.el
        [ Font.size << Basics.round <| theme.fontRythm -1
        , Font.light
        , Font.family
            [ Font.typeface "Roboto"
            ]
        ]
        << Element.text


{-| Show the number of NFTs in each state, and communicate the meaning of colors
to the users.
-}
nftCountsBar :
    DesignSystem
    -> SaleStateCounts
    -> Element msg
nftCountsBar designSystem counts =
    let
        theme =
            designSystem.theme

        nftStateColors =
            theme.colorScheme.nftState

        countPill : ColorPair -> String -> Int -> Element msg
        countPill colorPair name count =
            Element.el
                [ Background.color colorPair.color
                , Font.color colorPair.text
                , Border.rounded 16
                , roboto
                , Font.size << round <| theme.fontRythm -1
                , Element.paddingXY (round <| theme.spacingRythm 1) (round <| theme.spacingRythm 0)
                ]
            <|
                Element.text (name ++ ": " ++ String.fromInt count)

        isMobile =
            designSystem.device.class == DesignSystem.Responsiveness.Phone

        ifNotZero n elem =
            if n > 0 then
                elem

            else
                Element.none
    in
    Element.row
        [ Element.spacing (round <| theme.spacingRythm 1)
        ]
        [ ifNotZero counts.total <| countPill nftStateColors.other "Total" counts.total
        , ifNotZero counts.available <| countPill nftStateColors.available "Available" counts.available
        , ifNotZero counts.reserved <| countPill nftStateColors.reserved "Reserved" counts.reserved
        , ifNotZero counts.sold <| countPill nftStateColors.sold "Sold" counts.sold
        , if isMobile then
            Element.none

          else
            countPill nftStateColors.other "Not For Sale" counts.notForSale
        , if not isMobile && counts.error > 0 then
            countPill nftStateColors.error "Error" counts.error

          else
            Element.none
        ]



--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------


mapSession : (Session -> Session) -> Model -> Model
mapSession f model =
    { model | session = f model.session }


toSession : Model -> Session
toSession model =
    model.session
