module Components.Pagination exposing
    ( PaginationInfo, paginate, paginateForward
    , paginateBackward, view
    )

{-| The pagination component exposes a generic functions to implement pagination
on top of.


# Pagination

@docs PaginationInfo, paginate, paginateForward, paginateBackward view

-}

import DesignSystem.Fonts exposing (roboto)
import DesignSystem.Theme exposing (Theme)
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FeatherIcons
import Icons



--------------------------------------------------------------------------------
-- type
--------------------------------------------------------------------------------


type alias PaginationInfo =
    { pageStart : Maybe Int, pageEnd : Maybe Int, total : Int }



--------------------------------------------------------------------------------
-- view
--------------------------------------------------------------------------------


{-| A simple pagination bar with back and forward buttons.
-}
view :
    Theme
    ->
        { info : PaginationInfo
        , goBack : msg
        , goForward : msg
        }
    -> Element msg
view theme page =
    let
        pageStart =
            Maybe.withDefault "error" <| Maybe.map String.fromInt page.info.pageStart

        pageEnd =
            Maybe.withDefault "error" <| Maybe.map String.fromInt page.info.pageEnd

        infoString =
            pageStart ++ " - " ++ pageEnd ++ " of " ++ String.fromInt page.info.total

        canMoveForward =
            case page.info.pageEnd of
                Nothing ->
                    False

                Just n ->
                    n < page.info.total

        canMoveBackward =
            case page.info.pageStart of
                Nothing ->
                    False

                Just n ->
                    n > 1
    in
    Element.row
        [ Element.width Element.fill
        ]
        [ Input.button
            [ if canMoveBackward then
                Font.color theme.colorScheme.dark.color

              else
                Font.color theme.colorScheme.textSubtle
            , Region.description "Page backwards"
            , Element.alignLeft
            ]
            { onPress =
                if canMoveBackward then
                    Just <| page.goBack

                else
                    Nothing
            , label = Icons.toElement (theme.fontRythm 3) FeatherIcons.chevronLeft
            }
        , Element.el
            [ roboto
            , Font.size << round <| theme.fontRythm 0
            , Font.color theme.colorScheme.textBody
            , Element.centerX
            ]
          <|
            Element.text infoString
        , Input.button
            [ if canMoveForward then
                Font.color theme.colorScheme.dark.color

              else
                Font.color theme.colorScheme.textSubtle
            , Region.description "Page forwards"
            , Element.alignRight
            ]
            { onPress =
                if canMoveForward then
                    Just <| page.goForward

                else
                    Nothing
            , label = Icons.toElement (theme.fontRythm 3) FeatherIcons.chevronRight
            }
        ]



--------------------------------------------------------------------------------
-- functions
--------------------------------------------------------------------------------


{-| Given a page size, a page number and a list of items this function will pageinate it for you.
returning not only the items on the page but also the information to display to the user
-}
paginate : { pageSize : Int, page : Int } -> List a -> ( List a, PaginationInfo )
paginate { pageSize, page } data =
    let
        pageData =
            if page < 0 || pageSize < 0 then
                []

            else
                List.take pageSize <|
                    List.drop (pageSize * page) <|
                        data

        total =
            List.length data

        pageStart =
            page * pageSize + 1

        mPageStart =
            if pageSize > 0 && pageStart > 0 && page >= 0 && pageStart <= total then
                Just <| pageStart

            else
                Nothing

        mPageEnd =
            mPageStart
                |> Maybe.andThen
                    (\start ->
                        let
                            pageEnd =
                                start + (List.length pageData - 1)
                        in
                        if pageEnd <= total then
                            Just pageEnd

                        else
                            Nothing
                    )
    in
    if total == 0 then
        ( [], PaginationInfo (Just 0) (Just 0) 0 )

    else
        ( pageData, PaginationInfo mPageStart mPageEnd total )


{-| Paginate forward. Needs to know the total number of items it is paginating over.
-}
paginateForward : { pageSize : Int, page : Int, end : Int } -> Maybe Int
paginateForward p =
    if (p.page + 1) * p.pageSize + 1 <= p.end && p.page + 1 >= 0 && p.pageSize > 0 then
        Just <| p.page + 1

    else
        Nothing


{-| Paginate backward. Needs to know the total number of items it is paginating over.
-}
paginateBackward : { pageSize : Int, page : Int, end : Int } -> Maybe Int
paginateBackward p =
    if (p.page - 1) * p.pageSize + 1 <= p.end && (p.page - 1) >= 0 && p.pageSize > 0 then
        Just <| p.page - 1

    else
        Nothing
