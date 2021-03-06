module App exposing (..)

{-

   An app showcasing XSRF authentication using Servant-Auth

-}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html as Html
import Http as Http
import Http.XSRF as XSRF
import Json.Decode as D
import Json.Encode as E



-- main parametrised to D.Value to receive the document object


main : Program D.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Initialize the elm runtime with the document object


init : D.Value -> ( Model, Cmd Msg )
init cookies =
    -- initModel sets a value of Model containing document object
    ( initModel cookies
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Types


type alias Model =
    { username : String
    , password : String
    , showPassword : Bool
    , document : D.Value
    , output : String
    }


type Msg
    = ChangeUsername String
    | ChangePassword String
    | ShowPassword Bool
    | Login String String
    | LoginResponse (Result Http.Error ())
    | GetName
    | GetEmail
    | ReceivedName (Result Http.Error String)
    | ReceivedEmail (Result Http.Error String)


initModel : D.Value -> Model
initModel document =
    { username = ""
    , password = ""
    , showPassword = False
    , document = document
    , output = ""
    }


initCmd : Cmd Msg
initCmd =
    Cmd.none



-- View
-- I'll be using mdgriffith/elm-ui because its awesome!!


view : Model -> Html.Html Msg
view model =
    layout
        [ width fill
        , height fill
        ]
    <|
        viewEl model


viewEl : Model -> Element Msg
viewEl model =
    column
        [ centerX
        , width <| px 500
        , spacing 20
        , paddingXY 0 20
        ]
        [ usernameInput model
        , pwdInput model
        , showPassword model
        , loginButton model
        , cookieNotigfication model
        , nameButton
        , emailButton
        , showOutput model
        ]



-- Common attrributes for all my buttons


buttonAttributes : List (Attribute Msg)
buttonAttributes =
    [ width fill
    , height <| px 50
    , Background.color <| rgb255 200 200 200
    , Font.center
    ]


usernameInput : Model -> Element Msg
usernameInput model =
    Input.text
        [ width fill ]
        { onChange = ChangeUsername
        , text = model.username
        , placeholder = Nothing
        , label = Input.labelLeft [ width <| px 120 ] <| text "User name"
        }


pwdInput : Model -> Element Msg
pwdInput model =
    Input.newPassword
        [ width fill ]
        { onChange = ChangePassword
        , text = model.password
        , placeholder = Nothing
        , label = Input.labelLeft [ width <| px 120 ] <| text "Password"
        , show = model.showPassword
        }


showPassword : Model -> Element Msg
showPassword model =
    Input.checkbox
        []
        { onChange = ShowPassword
        , icon = Input.defaultCheckbox
        , checked = model.showPassword
        , label = Input.labelRight [] <| text "Show password"
        }


loginButton : Model -> Element Msg
loginButton model =
    Input.button
        buttonAttributes
        { onPress = Just <| Login model.username model.password
        , label = text "Login"
        }


cookieNotigfication : Model -> Element Msg
cookieNotigfication model =
    -- In a real app, you'd have a way to keep track if the user is
    -- authenticated or not
    case token "XSRF-TOKEN=" model.document of
        Just _ ->
            el
                [ width fill
                , height <| px 50
                , Background.color <| rgb255 116 205 128
                ]
            <|
                el [ centerY, centerX ] <|
                    text "Authenticated"

        Nothing ->
            el
                [ width fill
                , height <| px 50
                , Background.color <| rgb255 255 65 65
                ]
            <|
                el [ centerY, centerX ] <|
                    text "Not Authenticated"


nameButton : Element Msg
nameButton =
    Input.button
        buttonAttributes
        { onPress = Just <| GetName
        , label = text "Get Name"
        }


emailButton : Element Msg
emailButton =
    Input.button
        buttonAttributes
        { onPress = Just <| GetEmail
        , label = text "Get Email"
        }


showOutput : Model -> Element Msg
showOutput model =
    el
        [ width fill
        , height <| px 50
        , Background.color <| rgb255 252 153 131
        ]
    <|
        el [ centerY, centerX ] <|
            text model.output



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUsername str ->
            ( { model | username = str }
            , Cmd.none
            )

        ChangePassword str ->
            ( { model | password = str }
            , Cmd.none
            )

        ShowPassword flag ->
            ( { model | showPassword = flag }
            , Cmd.none
            )

        Login username password ->
            ( model
            , login username password
            )

        LoginResponse _ ->
            ( { model
                | username = ""
                , password = ""
              }
            , Cmd.none
            )

        GetName ->
            ( model
            , getNameRequest model
            )

        GetEmail ->
            ( model
            , getEmailRequest model
            )

        ReceivedName rlt ->
            case rlt of
                Err err ->
                    ( model, Cmd.none )

                Ok name ->
                    ( { model | output = name }, Cmd.none )

        ReceivedEmail rlt ->
            case rlt of
                Err err ->
                    ( model, Cmd.none )

                Ok email ->
                    ( { model | output = email }, Cmd.none )



-- Requests
-- Unprotected login request
-- Notice the use of Http.post for unprotected requests


login : String -> String -> Cmd Msg
login username password =
    Http.post
        { url = "http://localhost:4000/login"
        , body = Http.jsonBody <| encodeLogin username password
        , expect = Http.expectWhatever LoginResponse
        }



-- Protected request for the authenticated user's name
-- Notice the use of XSRF.get for protected requests


getNameRequest : Model -> Cmd Msg
getNameRequest model =
    XSRF.get
        { url = "http://localhost:4000/name"
        , expect = Http.expectJson ReceivedName D.string
        , xsrfHeaderName = "X-XSRF-TOKEN"
        , xsrfToken = token "XSRF-TOKEN=" model.document
        }



-- Protected request for the authenticated user's email


getEmailRequest : Model -> Cmd Msg
getEmailRequest model =
    XSRF.get
        { url = "http://localhost:4000/email"
        , expect = Http.expectJson ReceivedEmail D.string
        , xsrfHeaderName = "X-XSRF-TOKEN"
        , xsrfToken = token "XSRF-TOKEN=" model.document
        }



-- JSON


encodeLogin : String -> String -> E.Value
encodeLogin username password =
    E.object
        [ ( "username", E.string username )
        , ( "password", E.string password )
        ]


token : String -> D.Value -> Maybe String
token name value =
    let
        decodeCookie =
            D.field "cookie" D.string

        str =
            D.decodeValue decodeCookie value

        split a =
            String.split ";" a

        filtered a =
            List.filter (String.startsWith name) [ a ]

        head a =
            List.head <| filtered a

        trimmed a =
            Maybe.map (String.dropLeft (String.length name)) <| head a
    in
    case str of
        Err err ->
            Nothing

        Ok cookie ->
            trimmed cookie
