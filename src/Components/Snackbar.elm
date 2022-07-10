module Components.Snackbar exposing
    ( LifeSpan
    , Model
    , Msg
    , Snackbar
    , add
    , error
    , inform
    , init
    , peek
    , subscriptions
    , update
    , view
    )

{-| Exposing snackbar elements that are used to inform the user of event that
has occured within the app.
-}

import Browser.Events
import DesignSystem exposing (DesignSystem)
import DesignSystem.Fonts exposing (roboto)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Time



--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------


type Severity
    = Error
    | Inform


type LifeSpan
    = LifeSpan Int


lifespanFour : LifeSpan
lifespanFour =
    LifeSpan 4


lifespanFive : LifeSpan
lifespanFive =
    LifeSpan 5


lifespanSix : LifeSpan
lifespanSix =
    LifeSpan 6


lifespanSeven : LifeSpan
lifespanSeven =
    LifeSpan 7


lifespanEight : LifeSpan
lifespanEight =
    LifeSpan 8


{-| Check if a snackbar has stayed its allotted time.
-}
shouldRemoveSnack : LifeSpan -> Time.Posix -> Time.Posix -> Bool
shouldRemoveSnack (LifeSpan seconds) createdAt currentTime =
    let
        diff =
            Time.posixToMillis currentTime - Time.posixToMillis createdAt
    in
    diff > seconds * 1000


type alias Snackbar =
    { text : String
    , severity : Severity
    , lifeSpan : LifeSpan
    }


inform : String -> Snackbar
inform s =
    Snackbar s Inform lifespanFour


error : String -> Snackbar
error s =
    Snackbar s Error lifespanEight


{-| The snackbar is a queue. We only show one snackbar at a time.
-}
type Model
    = Model (List Snackbar) (Maybe Time.Posix)


{-| Initial snackbar model
-}
init : Model
init =
    Model [] Nothing


{-| Add a snackbar to the back of the queue.

It does not allow for duplicate snackbars! It will
only reset the timer in that case

-}
add : Snackbar -> Model -> Model
add snackbar (Model snackbars s) =
    if List.isEmpty snackbars then
        Model [ snackbar ] Nothing

    else if List.any ((==) snackbar) snackbars then
        if List.head snackbars == Just snackbar then
            Model snackbars Nothing

        else
            Model snackbars s

    else
        Model (snackbars ++ [ snackbar ]) s


{-| Look at the current active snackbar
-}
peek : Model -> Maybe Snackbar
peek (Model snackbars _) =
    List.head snackbars


{-| Check if there are any snackbars
-}
isEmpty : Model -> Bool
isEmpty (Model snackbars _) =
    List.isEmpty snackbars



--------------------------------------------------------------------------------
-- Messages
--------------------------------------------------------------------------------


type Msg
    = OnAnimationFrame Time.Posix



--------------------------------------------------------------------------------
-- SubScription
--------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    if isEmpty model then
        Sub.none

    else
        Browser.Events.onAnimationFrame OnAnimationFrame



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnAnimationFrame t ->
            case model of
                Model [] _ ->
                    Model [] Nothing

                Model snacks Nothing ->
                    Model snacks (Just t)

                Model (snack :: snacks) (Just f) ->
                    if shouldRemoveSnack snack.lifeSpan f t then
                        Model snacks (Just t)

                    else
                        model



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


{-| View a single snackbar.

NOTE: Snackbars only show up one at a time. If multiple events happen each
should be shown in turn.

-}
view :
    DesignSystem
    -> Snackbar
    -> Element msg
view designSystem { text, severity } =
    let
        theme =
            designSystem.theme

        nbrCols =
            if designSystem.grid.totalColumns > 8 then
                8

            else if designSystem.grid.totalColumns == 8 then
                6

            else
                4
    in
    Element.paragraph
        ([ Element.alignLeft
         , Element.spacing << round <| theme.spacingRythm 1
         , Border.rounded 16
         , Element.padding << round <| theme.spacingRythm 1
         , Element.width << Element.maximum (designSystem.widthOfNCol nbrCols) <| Element.shrink
         , Border.shadow theme.shadow
         , Font.size (round <| theme.fontRythm 1)
         , roboto
         ]
            ++ (case severity of
                    Inform ->
                        [ Background.color theme.colorScheme.accent
                        , Font.color theme.colorScheme.dark.color
                        ]

                    Error ->
                        [ Background.color theme.colorScheme.failure.color
                        , Font.color theme.colorScheme.failure.text
                        ]
               )
        )
        [ Element.text text ]
