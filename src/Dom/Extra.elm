module Dom.Extra exposing (..)

import Browser.Dom as Dom
import Task


{-| Reset the viewport to the top of the page.

    type Msg
        = NoOp

    resetViewPort NoOp

-}
resetViewport : msg -> Cmd msg
resetViewport msg =
    Task.perform (\_ -> msg) (Dom.setViewport 0 0)
