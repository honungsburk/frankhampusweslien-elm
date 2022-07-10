module HelpersTests exposing (..)

import Expect
import Helpers
import Test exposing (..)


segmentEverySuite : Test
segmentEverySuite =
    describe "Helpers.segmentEvery"
        [ test "Can handle -1" <|
            \_ ->
                Expect.equal (Helpers.segmentEvery -1 [ 1, 2, 3 ]) []
        , test "Can handle 0" <|
            \_ ->
                Expect.equal (Helpers.segmentEvery -1 [ 1, 2, 3 ]) []
        , test "Can handle the empty list" <|
            \_ ->
                Expect.equal (Helpers.segmentEvery 2 []) []
        , test "Can handle when lengths aligne" <|
            \_ ->
                Expect.equal (Helpers.segmentEvery 2 [ 1, 2, 3, 4 ]) [ [ 1, 2 ], [ 3, 4 ] ]
        , test "Can handle when lengths don't aligne " <|
            \_ ->
                Expect.equal (Helpers.segmentEvery 3 [ 1, 2, 3, 4 ]) [ [ 1, 2, 3 ], [ 4 ] ]
        ]
