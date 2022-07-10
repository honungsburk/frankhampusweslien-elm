module MimeTypeTest exposing (..)

import Expect
import MimeType exposing (MimeImage(..), MimeType(..))
import Test exposing (..)
import MimeType exposing (allMimeTypes)
import Json.Decode

roundTripSuite : Test
roundTripSuite =
    let
        roundTrip : MimeType -> Test
        roundTrip mime =
            let
                mimeString = MimeType.toString mime
            in
            test mimeString <|
                \_ ->
                    Expect.equal
                        (MimeType.parseMimeType mimeString )
                        (Ok mime)
    in
    describe "MimeType.toString >> MimeType.parseMimeType"
        <| List.map roundTrip allMimeTypes


encodeDecodeRoundTripSuite : Test
encodeDecodeRoundTripSuite =
    let
        roundTrip : MimeType -> Test
        roundTrip mime =
            test (MimeType.toString mime) <|
                \_ ->
                    Expect.equal
                        (mime |> MimeType.encoder >> Json.Decode.decodeValue MimeType.decoder )
                        (Ok mime)
    in
    describe "MimeType.encoder >> MimeType.Decoder"
        <| List.map roundTrip allMimeTypes
