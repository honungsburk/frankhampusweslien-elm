module Art.Sale exposing
    ( Status(..)
    , available
    , bought
    , boughtFor
    , dmIfInterested
    , fuzz
    , notForSale
    , saleNoLongerLocked
    , view
    )

import Cardano.Ada as Ada
import Cardano.Address
import DesignSystem exposing (DesignSystem)
import DesignSystem.Fonts exposing (roboto)
import DesignSystem.Theme exposing (Theme)
import Duration
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Fuzz
import Http exposing (Response)
import Icons
import Maybe exposing (withDefault)
import Route
import Time



--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------


type Status
    = Bought (Maybe Ada.Ada)
    | NotForSale
    | DMIfInterested
    | Gifted



--------------------------------------------------------------------------------
-- Fuzz
--------------------------------------------------------------------------------


fuzz : Fuzz.Fuzzer Status
fuzz =
    Fuzz.oneOf
        [ Fuzz.constant bought
        , Fuzz.constant notForSale
        , Fuzz.constant dmIfInterested
        ]



--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------


{-| A NFT that has been hardcoded as bought for an undisclosed price
-}
bought : Status
bought =
    Bought Nothing


{-| A NFT that has been hardcoded as bought for a disclosed price
-}
boughtFor : Int -> Status
boughtFor =
    Bought << Ada.adaSafe


{-| Artworks not for sale
-}
notForSale : Status
notForSale =
    NotForSale


{-| Part of my private collection and you have to DM me if you want one.
-}
dmIfInterested : Status
dmIfInterested =
    DMIfInterested



--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------


{-| Check if the current sale is locked to one use.

This is usefull to reset the page when the time for the lock runs out

-}
saleNoLongerLocked : Status -> Time.Posix -> Bool
saleNoLongerLocked status _ =
    case status of
        _ ->
            False


{-| Check whether or not an Art work is available for sale.

    available Bought == False

-}
available : Status -> Bool
available status =
    case status of
        DMIfInterested ->
            True

        _ ->
            False



--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------


type alias Config msg =
    { designSystem : DesignSystem
    , status : Status
    , currentTime : Maybe Time.Posix
    , copyToClipboard : String -> msg
    }



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Config msg -> Element msg
view config =
    let
        theme =
            config.designSystem.theme

        designSystem =
            config.designSystem
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing << Basics.round <| designSystem.theme.spacingRythm 1
        ]
        [ purchaseHeader designSystem "Purchase"
        , Element.el [ Element.centerX ] <|
            case config.status of
                Gifted ->
                    Element.el
                        [ roboto
                        , Font.size << Basics.round <| designSystem.theme.fontRythm 1
                        , Font.bold
                        , Font.color designSystem.theme.colorScheme.textBody
                        ]
                    <|
                        Element.text "Gifted to a member of the Cardano community!"

                Bought mPrice ->
                    Element.el
                        [ roboto
                        , Font.size << Basics.round <| designSystem.theme.fontRythm 1
                        , Font.bold
                        , Font.color designSystem.theme.colorScheme.textBody
                        ]
                    <|
                        Element.text <|
                            Maybe.withDefault "Sold!" <|
                                Maybe.map Ada.priceString mPrice

                DMIfInterested ->
                    Element.paragraph
                        [ roboto
                        , Font.size << Basics.round <| designSystem.theme.fontRythm 0
                        , Font.bold
                        , Font.color designSystem.theme.colorScheme.textBody
                        , Font.center
                        ]
                        [ Element.text "Sold as an NFT on the Cardano blockchain. DM on either twitter or instagram if interested."
                        ]

                NotForSale ->
                    Element.el
                        [ roboto
                        , Font.size << Basics.round <| designSystem.theme.fontRythm 1
                        , Font.bold
                        , Font.color designSystem.theme.colorScheme.textBody
                        ]
                    <|
                        Element.text "Not Currently For Sale."
        ]


errorText : Theme -> String -> Element msg
errorText theme =
    Element.el
        [ roboto
        , Font.size << Basics.round <| theme.fontRythm 1
        , Font.bold
        , Font.color theme.colorScheme.failure.color
        ]
        << Element.text


purchaseHeader : DesignSystem -> String -> Element msg
purchaseHeader designSystem =
    Element.el
        [ roboto
        , Font.size << Basics.round <| designSystem.theme.fontRythm 2
        , Font.bold
        , Element.width Element.fill
        , Font.center
        , Font.color designSystem.theme.colorScheme.dark.color
        ]
        << Element.text


timer : DesignSystem -> Time.Posix -> Time.Posix -> Element msg
timer designSystem expiresAt currentTime =
    let
        timeLeft =
            Duration.from currentTime expiresAt

        minutesLeft =
            Duration.inSeconds timeLeft
                |> (\s -> s / 60)
                |> floor

        secondsLeft =
            remainderBy 60
                << round
            <|
                Duration.inSeconds timeLeft
    in
    Element.paragraph
        [ roboto
        , Font.size << Basics.round <| designSystem.theme.fontRythm 0
        , if Duration.inSeconds timeLeft > 0 then
            Font.color designSystem.theme.colorScheme.textSubtle

          else
            Font.color designSystem.theme.colorScheme.failure.color
        ]
        [ Element.text "Locked for: "
        , if minutesLeft > 0 then
            Element.text << (\m -> m ++ "m ") <| String.fromInt minutesLeft

          else
            Element.none
        , if secondsLeft > 0 then
            Element.text << (\s -> s ++ "s") <| String.fromInt secondsLeft

          else
            Element.text "0s"
        ]


price : DesignSystem -> String -> Element msg
price designSystem ada =
    Element.paragraph
        []
        [ subtleBody designSystem "Ada to send: "
        , Element.el
            [ roboto
            , Font.size << Basics.round <| designSystem.theme.fontRythm 0
            , Font.color designSystem.theme.colorScheme.textBody
            , Font.bold
            ]
          <|
            Element.text <|
                ada
                    ++ " ADA"
        ]


subtleBody : DesignSystem -> String -> Element msg
subtleBody designSystem =
    Element.el
        [ roboto
        , Font.size << Basics.round <| designSystem.theme.fontRythm 0
        , Font.color designSystem.theme.colorScheme.textSubtle
        ]
        << Element.text


addressContainer : DesignSystem -> Cardano.Address.Address -> (String -> msg) -> Element msg
addressContainer designSystem addr copyToClipBoard =
    let
        theme =
            designSystem.theme
    in
    Element.row
        [ Element.width Element.fill
        , Border.rounded 16
        , Background.gradient theme.colorScheme.gradient
        , Element.padding << Basics.round <| theme.spacingRythm 1
        , Element.spacing << Basics.round <| theme.spacingRythm 1
        ]
        [ Element.paragraph
            [ roboto
            , Font.size
                << Basics.round
              <|
                theme.fontRythm 0
            , Font.color theme.colorScheme.white
            , Font.center
            , Element.width <| Element.maximum (designSystem.widthOfNCol 3) Element.fill -- Make responsive!
            ]
            [ Element.text <| Cardano.Address.get addr ]
        , Input.button
            [ Font.color theme.colorScheme.white
            ]
            { onPress = Just <| copyToClipBoard <| Cardano.Address.get addr
            , label = Icons.toElement (theme.fontRythm 3) FeatherIcons.copy
            }
        ]


link : DesignSystem -> String -> Element msg
link designSystem t =
    Element.link
        [ roboto
        , Font.size << Basics.round <| designSystem.theme.fontRythm 0
        , Font.color designSystem.theme.colorScheme.textSubtle
        , Font.underline
        , Font.bold
        ]
        { url = Route.routeToPath Route.FAQ
        , label = Element.text t
        }
