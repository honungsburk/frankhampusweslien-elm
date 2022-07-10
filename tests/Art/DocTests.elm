module Art.DocTests exposing (..)

import Expect
import Art.Doc as Doc
import Test exposing (..)
import Art.Sale as Sale 
import Fuzz 

safetyCheckSuite : Test
safetyCheckSuite =
    describe "Art.Doc.safetyCheck"
        [ test "Can handle empty list" <|
            \_ ->
                Expect.equal (Doc.safetyCheck []) []
        , test "Can handle a list with 1 element" <|
            \_ ->
                Expect.equal (Doc.safetyCheck [{ name = "a"}]) [{ name = "a"}]
        , test "Can handle a valid list" <|
            \_ ->
                let
                    valid = [{ name = "a"}, { name = "b"},{ name = "c"},{ name = "d"}]
                in Expect.equal (Doc.safetyCheck valid) valid
        , test "Can handle a invalid list" <|
            \_ ->
                let
                    valid = [{ name = "#a"}, { name = "##a"},{ name = "c"},{ name = "d"}]
                in Expect.equal (Doc.safetyCheck valid) []
        ]


type alias SimpleBuyStatus = { buyStatus : Sale.Status }

countSaleStatesSuit : Test
countSaleStatesSuit = 
    let
        init =  { total = 0
                , available = 0
                , reserved = 0
                , sold = 0
                , error = 0
                , notForSale = 0
                } 


        fuzzier = Fuzz.map (Doc.countSaleStates << List.map SimpleBuyStatus) <| Fuzz.list Sale.fuzz 
    in
    
    describe "Art.Doc.countSaleStates"
        [  fuzz fuzzier "No negative counts"
            <| \count -> Expect.true 
                            "No negative counts" 
                            ( count.total >= 0
                            && count.available >= 0
                            && count.reserved >= 0
                            && count.sold >= 0
                            && count.error >= 0
                            && count.notForSale >= 0
                            )
        ,   fuzz fuzzier "Sum must match"
            <| \count -> Expect.true            
                            "Sum didn't match" 
                            ( count.total == ( count.available 
                                            + count.reserved
                                            + count.sold
                                            + count.error
                                            + count.notForSale
                                            )
                            )
        ]