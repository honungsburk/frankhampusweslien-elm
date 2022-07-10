module Cardano.Address exposing
    ( Address
    , address, shortAddress
    , encoder, decoder
    , fuzz
    , get
    )

{-| Contains tyeps and functions to model Ada.


# Type

@docs Address


# Functions

@docs address, shortAddress, fullAddress


# Encoder & Decoder

@docs encoder, decoder


# Fuzz

@docs fuzz

-}

import Fuzz
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------


{-| A Shelly era address on the Cardano network.
-}
type Address
    = Address String



--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------


{-| A smart constructor doing a local check to see if the address follows the
allowed format: it starts with "addr1..." a.k.a. it's a Shelley era wallet.

    address "addre1asdjnaaksdnaksnd" == Just (Address "addre1asdjnaaksdnaksnd")

    address "ad1asdasdjnaaksdnaksnd" == Nothing

-}
address : String -> Maybe Address
address addr =
    -- The check for length 20 is arbitrarly chosen, but it prevents the shortAddress
    -- function from producing incorrect output. I think all address are longer then
    -- 20 characters.
    if String.startsWith "addr1" addr && String.length addr > 20 then
        Just <| Address addr

    else
        Nothing


{-| Create a shortned representation of the address that is easier for a human
to check.

    shortAddress (Address "addr1asdaksjdnakjsdn") == "addr1asdak...jsdn"

-}
shortAddress : Address -> String
shortAddress (Address addr) =
    String.left 9 addr ++ "..." ++ String.right 4 addr


{-| Pull out the full address.

    shortAddress (Address "addr1asdaksjdnakjsdn") == "addr1asdaksjdnakjsdn"

-}
get : Address -> String
get (Address addr) =
    addr



--------------------------------------------------------------------------------
-- Fuzz
--------------------------------------------------------------------------------


{-| This fuzzer is bad bad I can't be bothered to implement a real one right now.
TODO: Implement real fuzzer.
-}
fuzz : Fuzz.Fuzzer Address
fuzz =
    Fuzz.constant <| Address "addr1abc17872av872asbdjahbsd9823asdb2173y"



--------------------------------------------------------------------------------
-- Encoder & Decoder
--------------------------------------------------------------------------------


encoder : Address -> Value
encoder =
    Encode.string << get


decoder : Decoder Address
decoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case address s of
                    Just addr ->
                        Decode.succeed addr

                    Nothing ->
                        Decode.fail "Invalid Cardano Address"
            )
