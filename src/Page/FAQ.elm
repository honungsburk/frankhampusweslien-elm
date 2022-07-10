module Page.FAQ exposing (Model, Msg, init, mapSession, toSession, update, view)

import Components.Hairline exposing (hairline)
import DesignSystem exposing (DesignSystem)
import DesignSystem.Fonts exposing (roboto)
import DesignSystem.Responsiveness as Resp exposing (Device, Grid)
import DesignSystem.Theme exposing (Theme)
import Element exposing (Element, column, el, row, spacingXY, text)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import FeatherIcons
import Html
import Icons
import Session exposing (Session)
import Set exposing (Set)



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { session : Session
    , openQA : Set String
    }


init : Session -> ( Model, Cmd msg )
init session =
    ( Model session Set.empty, Cmd.none )


mapSession : (Session -> Session) -> Model -> Model
mapSession f model =
    { model | session = f model.session }



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = OpenQA String
    | CloseQA String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenQA key ->
            ( { model | openQA = Set.insert key model.openQA }, Cmd.none )

        CloseQA key ->
            ( { model | openQA = Set.remove key model.openQA }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> { title : String, content : Element Msg }
view model =
    { title = "FAQ"
    , content = Lazy.lazy mainView model
    }


mainView : Model -> Element Msg
mainView model =
    let
        theme =
            model.session.designSystem.theme

        faqB =
            faqBlock theme model.openQA

        faqPara : List (Element msg) -> Element msg
        faqPara =
            Element.paragraph
                [ Font.color theme.colorScheme.textBody
                , Font.size << Basics.round <| theme.fontRythm 0
                , roboto
                ]

        totalCols =
            model.session.designSystem.grid.totalColumns

        nbrCols =
            if totalCols > 8 then
                8

            else if totalCols == 8 then
                6

            else
                4
    in
    Element.column
        [ Element.width << Element.px <| model.session.designSystem.widthOfNCol nbrCols
        , Element.centerX
        , Element.spacing << Basics.round <| theme.spacingRythm 3
        , Element.paddingXY 0 << Basics.round <| theme.spacingRythm 3
        ]
        [ title theme "FAQ"
        , hairline theme 2
        , faqB
            { titleText = "What happens when I send my ADA to the purchase address?"
            , answer =
                faqPara
                    [ text "The server will register your purchase and then send the specified NFT back to your wallet. "
                    , text "It is using "
                    , link theme
                        { url = "https://pro.nft-maker.io/"
                        , label = "nft-maker"
                        }
                    , text " behind the scenes, which is built by the guy responsible for integrating payments into the "
                    , link theme
                        { url = "https://www.unsigs.com/"
                        , label = "unsigned algorithms website"
                        }
                    , text "."
                    ]
            }
        , faqB
            { titleText = "Which wallets can I use?"
            , answer =
                faqPara
                    [ text "You can Daedalus or Yoroi. "
                    , text "You can "
                    , el [ Font.bold ] <| text "not"
                    , text " use a wallet on an exchange, they do "
                    , el [ Font.bold ] <| text "not"
                    , text " support native tokens."
                    ]
            }
        , faqB
            { titleText = "Help!? I sent the ADA but I didn't recieve my NFT!"
            , answer =
                faqPara
                    [ text "It could take a little while before the transaction is processed so before panicking wait 10 minutes. "
                    , text "If you sent ADA to the correct address but didn't receive a NFT you should have been automatically refunded. "
                    , text "In any other case DM me through twitter, instagram or by email, and I will look into what happened."
                    ]
            }
        , faqB
            { titleText = "Why do the NFTs become reserved?"
            , answer =
                faqPara
                    [ text "It is so that people don't have to race each other. For the 20 minutes the address is available only you can buy it."
                    ]
            }
        , faqB
            { titleText = "What can I do with my NFT?"
            , answer =
                Element.textColumn
                    [ roboto
                    , Font.size << Basics.round <| theme.fontRythm 0
                    , Font.regular
                    , Font.color theme.colorScheme.textBody
                    , Element.width Element.fill
                    , Element.spacing << Basics.round <| theme.spacingRythm 2
                    ]
                    [ faqPara
                        [ text "The licensing for the NFT can be found in this "
                        , link theme
                            { url = "https://gitlab.com/HampusWeslien/fhw-nft-license"
                            , label = "git repo"
                            }
                        , text "."
                        ]
                    ]
            }
        ]


link : Theme -> { url : String, label : String } -> Element msg
link theme { url, label } =
    Element.link
        [ Font.color theme.colorScheme.primaryBrand.color
        ]
        { url = url
        , label = text label
        }


title : Theme -> String -> Element msg
title theme =
    el
        [ Font.color theme.colorScheme.dark.color
        , Font.size << Basics.round <| theme.fontRythm 6
        , Font.center
        , Font.bold
        , roboto
        , Element.centerX
        ]
        << text


faqBlock :
    Theme
    -> Set String
    ->
        { titleText : String
        , answer : Element msg
        }
    -> Element Msg
faqBlock theme openQA { titleText, answer } =
    let
        isOpen =
            Set.member titleText openQA
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing << Basics.round <| theme.spacingRythm 2
        ]
        [ Element.row
            [ Element.width Element.fill
            ]
            [ faqTitle theme titleText
            , Element.el [ Element.alignRight ] <| faqButton theme { isOpen = isOpen, id = titleText }
            ]
        , if isOpen then
            Element.map (always NoOp) answer

          else
            Element.none
        ]


faqButton : Theme -> { isOpen : Bool, id : String } -> Element Msg
faqButton theme { isOpen, id } =
    Input.button
        []
        { onPress =
            Just <|
                if isOpen then
                    CloseQA id

                else
                    OpenQA id
        , label =
            Icons.toElement (theme.fontRythm 3) <|
                if isOpen then
                    FeatherIcons.chevronDown

                else
                    FeatherIcons.chevronUp
        }


faqTitle : Theme -> String -> Element msg
faqTitle theme s =
    Element.paragraph
        [ Font.color theme.colorScheme.textBody
        , Font.size << Basics.round <| theme.fontRythm 3
        , Font.bold
        , roboto
        ]
        [ text s ]



--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------


toSession : Model -> Session
toSession model =
    model.session
