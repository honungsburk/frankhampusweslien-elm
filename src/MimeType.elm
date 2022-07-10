module MimeType exposing
    ( MimeType(..), allMimeTypes
    , parseMimeType, MimeImage(..)
    , fuzz
    , encoder, decoder
    , MimeVideo(..), hasResolution, toIndexable, toString
    )

{-| This module contains types for recognizing mime types. This is helpful
when displaying images so that you are doing it in the best way possible.


# MimeType

@docs MimeType, allMimeTypes


# Parsing function & toString

@docs parseMimeType, MimeImage


# SubTypes

@docs MimeImage


# Fuzzer

@docs fuzz


# Encoder & Decoder

@docs encoder, decoder

-}

import DecodeHelpers
import Fuzz
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------


{-| Models the major types image, audio, video and text that his project needs.
-}
type MimeType
    = Image MimeImage
    | Video MimeVideo


{-| Get all MimeTypes in a List
-}
allMimeTypes : List MimeType
allMimeTypes =
    let
        allImage =
            List.map
                Image
                [ Jpeg
                , Png
                , Svg
                ]

        allVideo =
            List.map
                Video
                [ MP4
                ]
    in
    allVideo ++ allImage


{-| Models the most common image subtypes
-}
type MimeImage
    = Jpeg
    | Png
    | Svg


{-| Models the most common video subtypes
-}
type MimeVideo
    = MP4


{-| Check if the mimetype would have a meaningfull interpreation of "resolution"
-}
hasResolution : MimeType -> Bool
hasResolution mimetype =
    case mimetype of
        Image Svg ->
            False

        _ ->
            True



--------------------------------------------------------------------------------
-- Parsing function & toString
--------------------------------------------------------------------------------


{-| Tries to parse the Mime type from a string.

    -- normal use of a type/subtype that is modelled:
    parseMimeType "image/jpeg" == Just (Image Jpeg)

    -- use with an empty string
    parseMimeType "" == Nothing

    -- use with something else
    parseMimeType "bla" == Nothing

-}
parseMimeType : String -> Result String MimeType
parseMimeType s =
    case String.toLower s of
        "image/svg+xml" ->
            Ok <| Image Svg

        "image/png" ->
            Ok <| Image Png

        "image/jpeg" ->
            Ok <| Image Jpeg

        "video/mp4" ->
            Ok <| Video MP4

        _ ->
            Err <| "I don't recognize '" ++ s ++ "' as a mimetype"


{-| Transforms a MimeType back to a string represenation.

    -- normal use of a type/subtype that is modelled:
    toString (Image Jpeg) == "image/jpeg"

-}
toString : MimeType -> String
toString mime =
    case mime of
        Image Svg ->
            "image/svg+xml"

        Image Png ->
            "image/png"

        Image Jpeg ->
            "image/jpeg"

        Video MP4 ->
            "video/mp4"


{-| Transforms a MimeType into a string that is easily searchable.
-}
toIndexable : MimeType -> String
toIndexable mime =
    case mime of
        Image Svg ->
            "svg xml"

        Image Png ->
            "png"

        Image Jpeg ->
            "jpeg jpg"

        Video MP4 ->
            "mp4"



--------------------------------------------------------------------------------
-- Fuzz
--------------------------------------------------------------------------------


fuzz : Fuzz.Fuzzer MimeType
fuzz =
    Fuzz.oneOf <| List.map Fuzz.constant allMimeTypes



--------------------------------------------------------------------------------
-- Encoder & Decoder
--------------------------------------------------------------------------------


encoder : MimeType -> Value
encoder =
    toString >> Encode.string


decoder : Decoder MimeType
decoder =
    Decode.string
        |> Decode.andThen (parseMimeType >> DecodeHelpers.resultToDecoder)
