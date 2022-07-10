module Element.Extra exposing
    ( onEnter
    , something, filler, fillers
    )

{-| This module contains needed extensions to the Elm-Ui library.


# Events

@docs onEnter


# Elements

@docs something, filler, fillers

-}

import Element exposing (Attribute, Element)
import Html
import Html.Events
import Json.Decode as Decode



--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------


{-| Triggers whenever the user presses the enter key
-}
onEnter : msg -> Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )



--------------------------------------------------------------------------------
-- Elements
--------------------------------------------------------------------------------


{-| When you need to fill out a grid to keep the spacing appropriate.
-}
fillers : Int -> List (Element msg)
fillers n =
    List.repeat n filler


{-| When you need to fill out a grid to keep the spacing appropriate.
-}
filler : Element msg
filler =
    Element.el [ Element.width Element.fill ] something


{-| Like Element.none but the opposite. It will render, but it contains nothing.
-}
something : Element msg
something =
    Element.html <| Html.div [] []
