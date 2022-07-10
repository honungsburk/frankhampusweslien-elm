module Cardano.AdaTests exposing (..)

import Expect
import Cardano.Ada as Ada
import Test exposing (..)
import Fuzz 


formatSuit : Test
formatSuit = 
    let
        testAdaFormat : String -> Int -> String -> Test
        testAdaFormat testString lovelace output =
            test testString <|
                \_ -> Ada.lovelace lovelace
                    |> Maybe.map Ada.format
                    |> Expect.equal (Just output)
    in
    describe "Cardano.Ada.format"
        [ testAdaFormat "Can handle one ada" 1000000 "1"
        , testAdaFormat "Can handle multiple ada" 11000000 "11"
        , testAdaFormat "Can handle zero" 0 "0"
        , testAdaFormat "Can handle non whole ada" 10000 "0.01"
        , testAdaFormat "Can handle fractional ada" 111110111 "111.110111"
        ]