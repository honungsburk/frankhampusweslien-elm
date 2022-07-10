module IPFSTests exposing (..)

import Expect
import IPFS
import Test exposing (..)
import Json.Decode


encodeDecodeSuite : Test
encodeDecodeSuite =
    let
        encodeDecode = Json.Decode.decodeValue IPFS.decoder 
               << IPFS.encoder 

    in
        describe "IPFS.(encode|decode)"
            [ fuzz 
                IPFS.fuzz
                "Can do roundtrip"
                <| \value -> Expect.equal (encodeDecode value) <| Ok <| value
            ]
