module Components.PaginationTests exposing (..)

import Components.Pagination exposing (paginate, PaginationInfo, paginateBackward, paginateForward) 
import Test exposing (..)
import Expect

paginateSuite : Test
paginateSuite = describe "Component.Pagination.paginate"
        [ test "Can handle pageSize 0" <|
            \_ ->
                Expect.equal (paginate { pageSize = 0, page = 1} [1, 2, 3]) ([], PaginationInfo Nothing Nothing 3)
        , test "Can handle page 0" <|
            \_ ->
                Expect.equal (paginate { pageSize = 1, page = 0} [1, 2, 3]) ([1], PaginationInfo (Just 1) (Just 1) 3)
        , test "Can handle middle page" <|
            \_ ->
                Expect.equal (paginate { pageSize = 1, page = 1} [1, 2, 3]) ([2], PaginationInfo (Just 2) (Just 2) 3)
        , test "Can handle pageSize to larger then number of items" <|
            \_ ->
                Expect.equal (paginate { pageSize = 4, page = 0} [1, 2, 3]) ([1, 2, 3], PaginationInfo (Just 1) (Just 3) 3)
        , test "Can handle pageSize beyond where the items are" <|
            \_ ->
                Expect.equal (paginate { pageSize = 4, page = 1} [1, 2, 3]) ([], PaginationInfo Nothing Nothing 3)
        , test "Can handle pageSize before where the items are" <|
            \_ ->
                Expect.equal (paginate { pageSize = 4, page = -1} [1, 2, 3]) ([], PaginationInfo Nothing Nothing 3)
        , test "Can handle last page to large" <|
            \_ ->
                Expect.equal (paginate { pageSize = 2, page = 1} [1, 2, 3]) ([3], PaginationInfo (Just 3) (Just 3) 3)
        , test "Can handle empty list" <|
            \_ ->
                Expect.equal (paginate { pageSize = 2, page = 0} []) ([], PaginationInfo (Just 0) (Just 0) 0)
        ]

paginateForwardSuit : Test
paginateForwardSuit 
    = describe "Component.Pagination.paginateForward"
        [ test "Can handle all zeroes" <|
            \_ ->
                Expect.equal (paginateForward {pageSize = 0, page = 0, end = 0 } ) Nothing
        , test "Can handle normal case" <|
            \_ ->
                Expect.equal (paginateForward {pageSize = 10, page = 0, end = 100 } ) (Just 1)
        , test "Can handle in middle case" <|
            \_ ->
                Expect.equal (paginateForward {pageSize = 10, page = 3, end = 100 } ) (Just 4)
        , test "Can handle subtle edge" <|
            \_ ->
                Expect.equal (paginateForward {pageSize = 10, page = 9, end = 100 } ) Nothing
        , test "Can handle obvious edge"<|
            \_ ->
                Expect.equal (paginateForward {pageSize = 10, page = 9, end = 95 } ) Nothing
        , test "Invalid in -> invalid out" <|
            \_ ->
                Expect.equal (paginateForward {pageSize = 10, page = 9, end = 50 } ) Nothing
        ]

paginateBackwardSuit : Test
paginateBackwardSuit 
    = describe "Component.Pagination.paginateBackward"
        [ test "Can handle all zeroes" <|
            \_ ->
                Expect.equal (paginateBackward {pageSize = 0, page = 0, end = 0 } ) Nothing
        , test "Can handle normal case" <|
            \_ ->
                Expect.equal (paginateBackward {pageSize = 10, page = 0, end = 100 } ) Nothing
        , test "Can handle in middle case" <|
            \_ ->
                Expect.equal (paginateBackward {pageSize = 10, page = 3, end = 100 } ) (Just 2)
        , test "Can handle subtle edge" <|
            \_ ->
                Expect.equal (paginateBackward {pageSize = 10, page = 9, end = 100 } ) (Just 8)
        , test "Can handle obvious edge"<|
            \_ ->
                Expect.equal (paginateBackward {pageSize = 10, page = 9, end = 95 } ) (Just 8)
        , test "Invalid in -> invalid out" <|
            \_ ->
                Expect.equal (paginateBackward {pageSize = 10, page = 9, end = 50 } ) Nothing
        , test "page 1 -> page 0" <|
            \_ ->
                Expect.equal (paginateBackward {pageSize = 24, page = 1, end = 197 } ) (Just 0)
        ]
