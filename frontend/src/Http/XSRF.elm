module Http.XSRF exposing
    ( authRequest
    , authGet
    , header
    , token
    )

import Http as Http

{-|
-}
authRequest :
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect msg
    , xsrfHeaderName : String
    , xsrfToken : Maybe String
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd msg
authRequest
    { method
    , headers
    , url
    , body
    , expect
    , xsrfHeaderName
    , xsrfToken
    , timeout
    , tracker
    } =
    Http.request
        { method = method
        , headers =
            header xsrfHeaderName xsrfToken
            :: headers
        , url = url
        , body = body
        , expect = expect
        , timeout = timeout
        , tracker = tracker
        }

{-|
-}
authGet :
    { url : String
    , expect : Http.Expect msg
    , xsrfHeaderName : String
    , xsrfToken : Maybe String
    }
    -> Cmd msg
authGet { url, expect, xsrfHeaderName, xsrfToken } =
    authRequest
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = expect
        , xsrfHeaderName = xsrfHeaderName
        , xsrfToken = xsrfToken
        , timeout = Nothing
        , tracker = Nothing
        }


header : String -> Maybe String -> Http.Header
header headerName cookie =
    Http.header headerName <| Maybe.withDefault "" cookie


token : String -> String -> Maybe String
token name str =
    let
        cookies =
            String.split ";" str

        filtered =
            List.filter (String.startsWith name) cookies
    in
    Maybe.map (String.dropLeft 11) <| List.head filtered
