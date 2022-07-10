module Session exposing
    ( Session, defaultSession
    , addSnackbar, updateSnackbar
    , addConnectionError, removeConnectionError
    )

{-| A session is the global state shared between all pages.


# Session

@docs Session, defaultSession


# Snackbar

@docs addSnackbar, updateSnackbar


# ConnectionError

@docs addConnectionError, removeConnectionError

-}

import Art.Doc
import Browser.Navigation as Nav
import Components.Snackbar as Snackbar
import DesignSystem exposing (DesignSystem, mkDesignSystem)
import Error
import Http



--------------------------------------------------------------------------------
-- Session
--------------------------------------------------------------------------------


type alias Session =
    { navKey : Nav.Key
    , designSystem : DesignSystem
    , artDocs : List Art.Doc.Doc
    , nftMakerConnectionError : Maybe String
    , snackbars : Snackbar.Model
    }


defaultSession : { window | height : Int, width : Int } -> Nav.Key -> Session
defaultSession window key =
    { navKey = key
    , designSystem = mkDesignSystem window
    , artDocs = Art.Doc.artContent
    , nftMakerConnectionError = Nothing
    , snackbars = Snackbar.init
    }



--------------------------------------------------------------------------------
-- SnackBar
--------------------------------------------------------------------------------


{-| Update the Snackbar
-}
updateSnackbar : Snackbar.Msg -> Session -> Session
updateSnackbar msg session =
    { session | snackbars = Snackbar.update msg session.snackbars }


{-| Add a snackbar state to a session
-}
addSnackbar : Snackbar.Snackbar -> Session -> Session
addSnackbar snackbar session =
    { session | snackbars = Snackbar.add snackbar session.snackbars }



--------------------------------------------------------------------------------
-- Connection Error Helpers
--------------------------------------------------------------------------------


addConnectionError : Session -> Http.Error -> Session
addConnectionError session httpErr =
    { session | nftMakerConnectionError = Just <| "Could not connect to pro.nft-maker: " ++ Error.httpErrorToString httpErr }


removeConnectionError : Session -> Session
removeConnectionError session =
    { session | nftMakerConnectionError = Nothing }
