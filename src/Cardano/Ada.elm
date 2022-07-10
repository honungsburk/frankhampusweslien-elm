module Cardano.Ada exposing
    ( Ada
    , lovelace, format, priceString
    , symbol
    , fuzz
    , lovelaceDecoder, lovelaceEncoder
    , adaSafe
    )

{-| Contains tyeps and functions to model Ada.


# Type

@docs Ada


# Functions

@docs ada, lovelace, format, priceString


# Symbol

@docs symbol


# Fuzz

@docs fuzz


# Encoder & Decoder

@docs adaEncoder, adaDecoder, lovelaceDecoder, lovelaceEncoder

-}

import Fuzz
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------


{-| Ada - The primary currency on the Cardano network.
-}
type Ada
    = Lovelace Int


{-| The amount of lovalace 1 ada is worth
-}
oneAdaInLovelace : Int
oneAdaInLovelace =
    1000000



--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------


{-| Createa string representing the price in ada
-}
priceString price =
    format price ++ " " ++ String.fromChar symbol


{-| Create Ada from Ada directly
-}
adaSafe : Int -> Maybe Ada
adaSafe n =
    lovelace <| n * oneAdaInLovelace


{-| A smart constructor that only allows positive Ada values (since we only sell).

Warning: performs rounding, Do not expose it!!!!

-}
adaUnsafe : Float -> Maybe Ada
adaUnsafe n =
    lovelace <| round <| toFloat oneAdaInLovelace * n


{-| A smart constructor that only allows positive lovalace values (since we only sell).
-}
lovelace : Int -> Maybe Ada
lovelace n =
    if n >= 0 then
        Just <| Lovelace n

    else
        Nothing


{-| Display the ada as a formated string.

    format (Ada 20000100) == "20.0001"

-}
format : Ada -> String
format (Lovelace n) =
    let
        infrontDot =
            n // oneAdaInLovelace

        behindDot =
            modBy oneAdaInLovelace n

        zeroPadded =
            String.padLeft 6 '0' <| String.fromInt behindDot

        ( smartPadded, _ ) =
            String.foldr
                (\c ( s, foundDigits ) ->
                    if (c /= '0' && not foundDigits) || foundDigits then
                        ( String.cons c s, True )

                    else
                        ( s, False )
                )
                ( "", False )
                zeroPadded
    in
    String.fromInt infrontDot
        ++ (if smartPadded /= "" then
                String.cons '.' smartPadded

            else
                ""
           )


{-| Get the amount of lovelace this ada represents.
-}
getLovelace : Ada -> Int
getLovelace (Lovelace n) =
    n


{-| Warning! This function does not preserve fractions of ada. Use getLovelace
instead.

This is why we are not gonna expose this function, since people will think this
is what they want but it is not.

-}
getAda : Ada -> Int
getAda (Lovelace n) =
    n // oneAdaInLovelace



--------------------------------------------------------------------------------
-- Symbol
--------------------------------------------------------------------------------


symbol : Char
symbol =
    '₳'



--------------------------------------------------------------------------------
-- Fuzz
--------------------------------------------------------------------------------


fuzz : Fuzz.Fuzzer Ada
fuzz =
    Fuzz.map Lovelace <| Fuzz.intRange 0 (oneAdaInLovelace * 100)



--------------------------------------------------------------------------------
-- Encoder & Decoder
--------------------------------------------------------------------------------


lovelaceEncoder : Ada -> Value
lovelaceEncoder =
    Encode.int << getLovelace


lovelaceDecoder : Decoder Ada
lovelaceDecoder =
    Decode.int
        |> Decode.andThen
            (\s ->
                lovelace s
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail <| "'" ++ String.fromInt s ++ " is an invalid Lovelace amount")
            )
