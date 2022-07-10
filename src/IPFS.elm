module IPFS exposing
    ( CID
    , cid
    , decoder
    , encoder
    , fuzz
    , ipfsDecoder
    , ipfsEncoder
    )

{-| This is just for type safty. I don't want us to accidentally think a cid is a
String. This module does not actually do anything to clever since it isn't yet needed.

We only do some basic checks to see if it is reasonle that it is a CID and add some
helper functions that can be used for testing etc. Feel free to make a more rigourous
implementation if your requirements need it.

-}

import DecodeHelpers
import Fuzz
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------


{-| A refrence CID refrence on IPFS (Content Address)

You can read up on CIDs (here)[<https://docs.ipfs.io/concepts/content-addressing/>][https://docs.ipfs.io/concepts/content-addressing/]

-}
type CID
    = CID String


{-| A smart constructor for CIDs
-}
cid : String -> Result String CID
cid =
    Ok << CID


{-| Get the hash from the CID
-}
getHash : CID -> String
getHash (CID s) =
    s



--------------------------------------------------------------------------------
-- Fuzz
--------------------------------------------------------------------------------


fuzz : Fuzz.Fuzzer CID
fuzz =
    Fuzz.constant <| CID "QmZ2mW76fV15N1XwKaihpQ7i9N1W28YQjDyowvv5rGfRR2"



-- I think using a contant should be fine
-- it doesn't make sense to shrink it and we get 99% of the benefit with this
-- very simple implementation
--------------------------------------------------------------------------------
-- Encode & Decode
--------------------------------------------------------------------------------


encoder : CID -> Value
encoder =
    Encode.string << getHash


decoder : Decoder CID
decoder =
    Decode.string
        |> Decode.andThen (cid >> DecodeHelpers.resultToDecoder)


{-| Same as the normal encoder but will prepend 'ipfs://' before the hash
-}
ipfsEncoder : CID -> Value
ipfsEncoder =
    getHash
        >> (\s -> "ipfs://" ++ s)
        >> Encode.string


{-| Same as the normal decoder but will remove the 'ipfs://' before the hash
-}
ipfsDecoder : Decoder CID
ipfsDecoder =
    Decode.string
        |> Decode.andThen
            (String.dropLeft 7
                >> cid
                >> DecodeHelpers.resultToDecoder
            )
