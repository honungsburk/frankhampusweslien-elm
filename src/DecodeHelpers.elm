module DecodeHelpers exposing (..)

{-| Conatains helper functions that you can use when creating JSON decoders.


# Transforms

@docs resultToDecoder, maybeToDecoder

-}

import Json.Decode as Decode exposing (Decoder)


{-| Transform a Result into a Decoder
-}
resultToDecoder : Result String a -> Decoder a
resultToDecoder result =
    case result of
        Ok a ->
            Decode.succeed a

        Err s ->
            Decode.fail s


{-| Transform a Maybe into a Decoder
-}
maybeToDecoder : String -> Maybe a -> Decoder a
maybeToDecoder err maybe =
    case maybe of
        Just a ->
            Decode.succeed a

        Nothing ->
            Decode.fail err
