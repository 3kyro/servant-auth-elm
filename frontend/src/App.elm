port module App exposing (..)


import Browser
import Element exposing (..)
import Element.Input as Input
import Html as Html
import Http as Http
import Json.Encode as E
import Json.Decode as D
import Http.XSRF as XSRF

main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> toElm CookieUpdate
        }

init : String -> ( Model, Cmd Msg )
init cookies = ( initModel cookies , initCmd )

port toElm : (String -> msg) -> Sub msg

type alias Model =
    { username : String
    , password : String
    , showPassword : Bool
    , cookies : String
    , email : String
    }

type Msg
    = ChangeUsername String
    | ChangePassword String
    | ShowPassword Bool
    | Login String String
    | LoginResponse (Result Http.Error ())
    | GetEmail
    | ReceivedEmail (Result Http.Error String)
    | CookieUpdate String
initModel : String -> Model
initModel cookies =
    { username = ""
    , password = ""
    , showPassword = False
    , cookies = cookies
    , email = ""
    }

initCmd : Cmd Msg
initCmd = Cmd.none

view : Model -> Html.Html Msg
view model =
    layout [] <| viewEl model

viewEl : Model -> Element Msg
viewEl model =
    column
        []
        [ nameInput model
        , pwdInput model
        , showPassword model
        , loginButton model
        , cookie model
        , emailButton
        , showEmail model
        ]

nameInput : Model -> Element Msg
nameInput model =
    Input.text
    []
    { onChange = ChangeUsername
    , text = model.username
    , placeholder = Nothing
    , label = Input.labelLeft [] <| text "User name"
    }

pwdInput : Model -> Element Msg
pwdInput model =
    Input.newPassword
    []
    { onChange = ChangePassword
    , text = model.password
    , placeholder = Nothing
    , label = Input.labelLeft [] <| text "Password"
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
    []
    { onPress = Just <| Login model.username model.password
    , label = text "Login"
    }

cookie : Model -> Element Msg
cookie model = text <| model.cookies

emailButton : Element Msg
emailButton =
    Input.button
    []
    { onPress = Just <| GetEmail
    , label = text "Get Email"
    }

showEmail : Model -> Element Msg
showEmail model =
    text <| model.email

update : Msg -> Model -> (Model , Cmd Msg)
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
            ( { model
                | username = ""
                , password = ""
              }
            , login username password
            )
        LoginResponse rlt ->
            ( model
            , Cmd.none
            )
        GetEmail ->
            ( model
            , getEmailRequest model
            )
        ReceivedEmail rlt ->
            case rlt of
                Err err -> (model, Cmd.none)
                Ok email -> ( { model | email = email}, Cmd.none)
        CookieUpdate str ->
            ( { model | cookies = str }
            , Cmd.none
            )
login : String -> String -> Cmd Msg
login username password =
    Http.post
        { url = "http://localhost:4000/login"
        , body = Http.jsonBody <| encodeLogin username password
        , expect = Http.expectWhatever LoginResponse
        }

getEmailRequest : Model -> Cmd Msg
getEmailRequest model =
    XSRF.authGet
        { url = "http://localhost:4000/email"
        , expect = Http.expectJson ReceivedEmail D.string
        , xsrfHeaderName = "X-XSRF-TOKEN"
        , xsrfToken = XSRF.token "XSRF-TOKEN=" model.cookies
        }

encodeLogin : String -> String -> E.Value
encodeLogin username password =
    E.object
        [ ("username", E.string username)
        , ("password", E.string password)
        ]


