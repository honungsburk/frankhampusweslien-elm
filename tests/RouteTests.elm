module RouteTests exposing (..)


import Expect
import Route
import Test exposing (..)
import Url

routeRoundTripFuzz : Test 
routeRoundTripFuzz =
    let
        roundTrip : Route.Route -> Maybe Route.Route
        roundTrip = Maybe.andThen Route.fromUrl << Url.fromString << (++) "https://localhost:8000" << Route.routeToPath
    in
    describe "Fuzz test: Route -> String -> URL -> Route"
        [ fuzz 
            Route.fuzz
            "Can do roundtrip"
            <| \route -> Expect.equal (roundTrip route) <| Just route
        ]