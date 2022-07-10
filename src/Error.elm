module Error exposing (httpErrorToString)

import Http


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadBody s ->
            "Bad Body - '" ++ s ++ "'"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus n ->
            "Bad Status - " ++ String.fromInt n

        Http.BadUrl s ->
            "Bad Url - '" ++ s ++ "'"
