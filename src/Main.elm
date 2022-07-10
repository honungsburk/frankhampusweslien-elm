port module Main exposing (..)

import Art.Doc
import Browser exposing (Document)
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Components.Snackbar as Snackbar
import Debug
import DesignSystem exposing (DesignSystem, mkDesignSystem)
import DesignSystem.Theme exposing (Theme)
import Dom.Extra
import Element exposing (Element)
import Html.Attributes as Attributes
import List.Extra
import Page
import Page.Art as Art
import Page.Artwork
import Page.Blank as Blank
import Page.FAQ as FAQ
import Page.Home
import Page.Me
import Page.NotFound as NotFound
import Route exposing (Route)
import Session exposing (Session)
import Url


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



--------------------------------------------------------------------------------
-- PORTS
--------------------------------------------------------------------------------


port copyToClipboard : String -> Cmd msg



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type Model
    = NotFound Session
    | Redirect Session
    | Me Session
    | FAQ FAQ.Model
    | Art Art.Model
    | Home Session
    | Artwork Page.Artwork.Model


type alias Flags =
    { height : Int, width : Int }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init window url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.defaultSession window navKey))


toSession : Model -> Session
toSession model =
    case model of
        Art subModel ->
            Art.toSession subModel

        NotFound session ->
            session

        Me session ->
            session

        FAQ subModel ->
            FAQ.toSession subModel

        Redirect session ->
            session

        Home session ->
            session

        Artwork subModel ->
            Page.Artwork.toSession subModel


mapSession : (Session -> Session) -> Model -> Model
mapSession f model =
    case model of
        Art subModel ->
            Art <| Art.mapSession f subModel

        NotFound session ->
            NotFound <| f session

        Me session ->
            Me <| f session

        FAQ subModel ->
            FAQ <| FAQ.mapSession f subModel

        Home session ->
            Home <| f session

        Redirect session ->
            Redirect <| f session

        Artwork subModel ->
            Artwork <| Page.Artwork.mapSession f subModel



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url
    | OnResize Int Int
    | GotFAQMsg FAQ.Msg
    | GotArtMsg Art.Msg
    | GotArtworkMsg Page.Artwork.Msg
    | NoOp
    | GotSnackbarMsg Snackbar.Msg



--| DismissSnackBar Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (toSession model).navKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            case ( Route.fromUrl url, model ) of
                -- This is for preventing going into the init of the Art page on each search
                ( Just (Route.Art newQuery), Art submodel ) ->
                    ( Art { submodel | query = newQuery }, Cmd.none )

                _ ->
                    changeRouteTo (Route.fromUrl url) model

        ( OnResize width height, _ ) ->
            ( mapSession (\session -> { session | designSystem = mkDesignSystem { width = width, height = height } }) model, Cmd.none )

        ( GotFAQMsg subMsg, FAQ subModel ) ->
            FAQ.update subMsg subModel |> updateWith FAQ GotFAQMsg

        ( GotArtMsg subMsg, Art subModel ) ->
            Art.update subMsg subModel |> updateWith Art GotArtMsg

        ( GotArtworkMsg subMsg, Artwork subModel ) ->
            Page.Artwork.update { copyToClipboard = copyToClipboard } subMsg subModel |> updateWith Artwork GotArtworkMsg

        -- (DismissSnackBar id, _) ->
        --     (mapSession (Session.removeSnackBarState id) model, Cmd.none)
        ( GotSnackbarMsg subMsg, _ ) ->
            ( mapSession (Session.updateSnackbar subMsg) model, Cmd.none )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize OnResize
        , Sub.map GotSnackbarMsg << Snackbar.subscriptions <| (toSession model).snackbars
        , case model of
            Artwork subModel ->
                Sub.map GotArtworkMsg <| Page.Artwork.subscriptions subModel

            _ ->
                Sub.none
        ]



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


options : Theme -> List Element.Option
options theme =
    let
        focusStyle =
            { borderColor = Nothing
            , backgroundColor = Nothing
            , shadow =
                Just
                    { color = theme.colorScheme.primaryBrand.color
                    , offset = ( 0, 0 )
                    , blur = 0
                    , size = 3
                    }
            }
    in
    [ Element.focusStyle focusStyle
    ]


view : Model -> Document Msg
view model =
    let
        viewPage toMsg page renderedPage =
            let
                session =
                    toSession model

                { title, body } =
                    Page.view session page <| renderedPage
            in
            { title = title
            , body =
                [ Element.layoutWith
                    { options = options session.designSystem.theme }
                    [ Element.inFront <|
                        Element.el
                            [ Element.centerX
                            , Element.alignBottom
                            , Element.paddingEach
                                { top = 0
                                , right = 0
                                , left = 0

                                -- 4 was choosen so as to not cover the social media links in the footer
                                , bottom = round <| session.designSystem.theme.spacingRythm 4
                                }
                            ]
                        <|
                            Element.map GotSnackbarMsg <|
                                Maybe.withDefault Element.none <|
                                    Maybe.map (Snackbar.view session.designSystem) <|
                                        Snackbar.peek session.snackbars
                    ]
                  <|
                    Element.map toMsg body
                ]
            }
    in
    case model of
        Redirect _ ->
            viewPage identity Page.Other <| Blank.view

        NotFound session ->
            viewPage identity Page.Other <| NotFound.view session.designSystem

        FAQ subModel ->
            viewPage GotFAQMsg Page.FAQ <| FAQ.view subModel

        Art subModel ->
            viewPage GotArtMsg Page.Art <| Art.view subModel

        Home session ->
            viewPage identity Page.Home <| Page.Home.view session.designSystem

        Me session ->
            viewPage identity Page.Me <| Page.Me.view session.designSystem

        Artwork subModel ->
            viewPage GotArtworkMsg Page.Other <| Page.Artwork.view subModel


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Me ->
            ( Me session, Cmd.none )

        Just (Route.Art query) ->
            Art.init session query |> updateWith Art GotArtMsg

        Just Route.FAQ ->
            FAQ.init session |> updateWith FAQ GotFAQMsg

        Just Route.Home ->
            ( Home session, Cmd.none )

        Just (Route.Artwork id) ->
            --Page.Artwork.init session id |> updateWith Artwork identity
            case List.Extra.find (\artwork -> artwork.cid == id) session.artDocs of
                Nothing ->
                    ( NotFound session, Dom.Extra.resetViewport NoOp )

                Just artwork ->
                    Page.Artwork.init session artwork |> updateWith Artwork GotArtworkMsg


{-| Helper function to wrap the states and messages of different pages
-}
updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )
