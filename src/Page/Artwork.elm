module Page.Artwork exposing
    ( Model
    , Msg
    , init
    , mapSession
    , subscriptions
    , toSession
    , update
    , view
    )

import Art.Doc exposing (Doc)
import Art.Group
import Art.Sale
import Cardano.Address
import Components.Hairline exposing (hairline)
import Components.Snackbar as Snackbar exposing (Snackbar)
import Debug
import DesignSystem exposing (DesignSystem)
import DesignSystem.Fonts as DFonts exposing (roboto)
import DesignSystem.Theme exposing (Theme)
import Dom.Extra
import Element exposing (Element, height, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy as Lazy
import Element.Region as Region
import FeatherIcons
import Html
import Html.Attributes
import Html.Events
import Http
import Icons
import Json.Encode as Encode
import MimeType
import Resolution
import Session exposing (Session)
import Time



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { session : Session
    , artDoc : Doc

    -- , nftMetadata : Maybe NFTMetadata
    , currentTime : Maybe Time.Posix
    }


init : Session -> Doc -> ( Model, Cmd Msg )
init session artDoc =
    ( Model session artDoc Nothing
    , Cmd.batch
        [ Dom.Extra.resetViewport (NoOP ())
        , Cmd.none
        ]
    )



--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------


type alias Config msg =
    { copyToClipboard : String -> Cmd msg
    }



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = NoOP ()


update : Config msg -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



--------------------------------------------------------------------------------
-- Subscriptions
--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> { title : String, content : Element Msg }
view model =
    { title = model.artDoc.name
    , content =
        Lazy.lazy columnLayout model
    }


{-| The columnLayout is for small + tall screens
-}
columnLayout : Model -> Element Msg
columnLayout model =
    let
        designSystem =
            model.session.designSystem

        theme =
            designSystem.theme
    in
    Element.column
        [ Element.spacing (designSystem.grid.spacingX * 2)
        , Element.paddingXY designSystem.grid.paddingX (Basics.round <| theme.spacingRythm 3)
        , Element.width Element.fill
        ]
    <|
        [ mediaDisplay designSystem model.artDoc
        , Element.el
            [ Element.width << Element.px <| designSystem.widthOfNCol 4
            , Element.centerX
            ]
          <|
            imageInfo model
        ]


mediaDisplay : DesignSystem -> Doc -> Element msg
mediaDisplay designSystem doc =
    let
        theme =
            designSystem.theme

        basic =
            Element.column
                [ Element.width Element.shrink
                , Element.centerX
                , Element.spacing (Basics.round <| theme.spacingRythm 2)
                ]
                [ image designSystem doc
                , Element.wrappedRow
                    [ Element.width Element.fill
                    , Element.spacing << round <| theme.spacingRythm 1
                    ]
                    [ mimeTypeInfo theme doc.mime
                    , if MimeType.hasResolution doc.mime then
                        resolutionInfo theme doc.resolution

                      else
                        Element.none
                    , Element.el [ Element.alignRight ] <| downLoadLink theme (Art.Doc.fullResSrc doc)
                    ]
                ]
    in
    (if not <| Art.Group.isMotion doc.group then
        Nothing

     else
        Just <|
            Element.column
                [ Element.width Element.shrink
                , Element.centerX
                , Element.spacing (Basics.round <| theme.spacingRythm 2)
                ]
                [ video designSystem doc (MimeType.Video MimeType.MP4) (Art.Doc.fullResSrc doc)
                , Element.wrappedRow
                    [ Element.width Element.fill
                    , Element.spacing << round <| theme.spacingRythm 1
                    ]
                    [ mimeTypeInfo theme doc.mime
                    , if MimeType.hasResolution doc.mime then
                        resolutionInfo theme doc.resolution

                      else
                        Element.none
                    , Element.el [ Element.alignRight ] <| downLoadLink theme (Art.Doc.fullResSrc doc)
                    ]
                ]
    )
        |> Maybe.withDefault basic



-- TODO: create nice "component"


video : DesignSystem -> Doc -> MimeType.MimeType -> String -> Element msg
video designSystem art mediaType link =
    let
        maxHeight =
            Basics.min
                (designSystem.fractionOfHeight 0.8)
                (designSystem.widthOfNCol designSystem.grid.totalColumns)

        maxWidth =
            maxHeight * art.resolution.width // art.resolution.height

        shadow =
            designSystem.theme.shadow

        dims =
            if Resolution.isInLandscape art.resolution then
                [ Element.width <| Element.maximum maxWidth Element.fill ]

            else
                [ Element.height <| Element.px maxHeight ]
    in
    Element.el ([ Element.centerX, Border.shadow shadow ] ++ dims) <|
        Element.html <|
            Html.video
                [ Html.Attributes.controls True
                , Html.Attributes.loop True
                , Html.Attributes.property "muted" <| Encode.bool True
                ]
                [ Html.source
                    [ Html.Attributes.src link
                    , Html.Attributes.type_ <| MimeType.toString mediaType
                    ]
                    []
                , Html.text "Your browser does not support the video tag. Update your browser."
                ]


image : DesignSystem -> Doc -> Element msg
image designSystem art =
    let
        maxHeight =
            Basics.min
                (designSystem.fractionOfHeight 0.8)
                (designSystem.widthOfNCol designSystem.grid.totalColumns)

        maxWidth =
            maxHeight * art.resolution.width // art.resolution.height

        shadow =
            designSystem.theme.shadow

        dims =
            if Resolution.isInLandscape art.resolution then
                [ Element.width <| Element.maximum maxWidth Element.fill ]

            else
                [ Element.height <| Element.px maxHeight ]
    in
    Element.image
        ([ Element.centerX, Border.shadow shadow ] ++ dims)
    <|
        { src = Maybe.withDefault "lol" <| Art.Doc.lowResSrc art -- TODO: Fix better default
        , description = art.shortDescription
        }


imageInfo : Model -> Element Msg
imageInfo model =
    let
        designSystem =
            model.session.designSystem
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing << Basics.round <| designSystem.theme.spacingRythm 3
        ]
        [ Element.column
            [ Element.spacing << Basics.round <| designSystem.theme.spacingRythm 2
            , Element.centerX
            ]
            [ name designSystem <| Art.Doc.displayName model.artDoc
            , Element.map NoOP <| description designSystem model.artDoc.description
            ]
        , hairline designSystem.theme 2
        , viewMetadata model
        ]


name : DesignSystem -> String -> Element msg
name designSystem t =
    Element.paragraph
        [ roboto
        , Font.size << Basics.round <| designSystem.theme.fontRythm 4
        , Font.bold
        , Font.center
        , Element.width Element.fill
        , Font.color designSystem.theme.colorScheme.dark.color
        ]
        [ Element.text t ]


description : DesignSystem -> List (Element msg) -> Element msg
description designSystem =
    Element.textColumn
        [ roboto
        , Font.size << Basics.round <| designSystem.theme.fontRythm 0
        , Font.regular
        , Font.color designSystem.theme.colorScheme.textBody
        , Font.center
        , Element.width Element.fill
        , Element.spacing << Basics.round <| designSystem.theme.spacingRythm 2
        ]


downLoadLink : Theme -> String -> Element msg
downLoadLink theme src =
    Element.download
        [ Font.color theme.colorScheme.dark.color
        , Region.description "Download full resolution image"
        ]
        { url = src
        , label = Icons.toElement (theme.fontRythm 3) FeatherIcons.download
        }



--------------------------------------------------------------------------------
-- Below MetaData
--------------------------------------------------------------------------------


pill : Theme -> String -> Element msg
pill theme =
    Element.el
        [ Background.color theme.colorScheme.dark.color
        , Font.color theme.colorScheme.dark.text
        , roboto
        , Font.size << round <| theme.fontRythm -1
        , Border.rounded 8
        , Element.paddingXY (round <| theme.spacingRythm 1) (round <| theme.spacingRythm 0)
        ]
        << Element.text


mimeTypeInfo : Theme -> MimeType.MimeType -> Element msg
mimeTypeInfo theme mimeType =
    pill theme <| MimeType.toString mimeType


resolutionInfo : Theme -> Resolution.Resolution -> Element msg
resolutionInfo theme { width, height } =
    pill theme <| String.fromInt width ++ "px by " ++ String.fromInt height ++ "px"



--------------------------------------------------------------------------------
-- More MetaData
--------------------------------------------------------------------------------


viewMetadata : Model -> Element msg
viewMetadata model =
    let
        designSystem =
            model.session.designSystem
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing << round <| designSystem.theme.spacingRythm 2
        ]
        [ Element.paragraph
            [ roboto
            , Font.size << Basics.round <| designSystem.theme.fontRythm 3
            , Font.bold
            , Font.center
            , Element.width Element.fill
            , Font.color designSystem.theme.colorScheme.dark.color
            ]
            [ Element.text "Metadata" ]
        , Element.column
            [ Element.centerX
            , Element.spacing << round <| designSystem.theme.spacingRythm 1
            ]
          <|
            List.map (policyIdPill designSystem) <|
                Art.Group.policyIds model.artDoc.group
        ]


policyIdPill : DesignSystem.DesignSystem -> String -> Element msg
policyIdPill designSystem policyId =
    let
        theme =
            designSystem.theme
    in
    Element.paragraph
        [ Background.color theme.colorScheme.dark.color
        , Font.color theme.colorScheme.dark.text
        , roboto
        , Element.width << Element.maximum (designSystem.widthOfNCol 4) <| Element.shrink
        , Font.size
            << round
            << theme.fontRythm
          <|
            -1
        , Border.rounded 8
        , Element.paddingXY (round <| theme.spacingRythm 1) (round <| theme.spacingRythm 0)
        ]
        -- Non-breaking space characters look better in the GUI
        [ Element.text <| "Policy\u{00A0}" ++ "ID:\u{00A0}" ++ policyId
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
