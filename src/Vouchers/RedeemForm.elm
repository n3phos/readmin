module Vouchers.RedeemForm exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, onClick, onSubmit)
import Vouchers.Model exposing (..)


type alias State =
    { values : Values }


newState : State
newState =
    State
        (Values "" "hello" "")


type alias Values =
    { username : String
    , password : String
    , partnerVoucherCode : String
    }


type Msg
    = OnInput


type OnInput
    = OnInputUsername String
    | OnInputPassword String
    | OnInputPartnerVoucherCode String
    | Submit


type Configuration msg
    = Configuration
        { toMsg : OnInput -> msg
        }


view : Configuration msg -> State -> Html msg
view (Configuration { toMsg }) state =
    Html.map toMsg <|
        Html.form
            [ style [ ( "margin-bottom", "5rem" ) ], onSubmit Submit ]
            ([ usernameInput
             , passwordInput
             , partnerVoucherCodeInput
             , redeemBtn
             ]
            )


update : (FormData -> Credentials -> Cmd msg) -> OnInput -> State -> ( State, Cmd msg )
update submitCmd msg state =
    let
        { values } =
            state
    in
        case msg of
            OnInputUsername x ->
                ( updateValues { values | username = x } state, Cmd.none )

            OnInputPassword x ->
                ( updateValues { values | password = x } state, Cmd.none )

            OnInputPartnerVoucherCode x ->
                ( updateValues { values | partnerVoucherCode = x } state, Cmd.none )

            Submit ->
                ( state, applySubmitToValues submitCmd state )


updateValues : Values -> State -> State
updateValues values state =
    { state | values = values }


applySubmitToValues : (FormData -> Credentials -> Cmd msg) -> State -> Cmd msg
applySubmitToValues submitCmd { values } =
    let
        formData =
            toFormData values
    in
        submitCmd formData (Credentials values.username values.password)


usernameInput : Html OnInput
usernameInput =
    div [ class "form-group row" ]
        [ label [ class "col-sm-2 control-label" ] [ text "Username" ]
        , div [ class "col-sm-6" ] [ input [ class "form-control", placeholder "Enter a Login", required True, type_ "text", onInput OnInputUsername ] [] ]
        ]


passwordInput : Html OnInput
passwordInput =
    div [ class "form-group row" ]
        [ label [ class "col-sm-2 control-label" ] [ text "Password" ]
        , div [ class "col-sm-6" ] [ input [ class "form-control", placeholder "Enter a Password", required True, type_ "password", onInput OnInputPassword ] [] ]
        ]


partnerVoucherCodeInput : Html OnInput
partnerVoucherCodeInput =
    div [ class "form-group row" ]
        [ label [ class "col-sm-2 control-label" ] [ text "Partner Voucher Code" ]
        , div [ class "col-sm-6" ] [ input [ class "form-control", placeholder "Enter a partner voucher code", required True, type_ "text", onInput OnInputPartnerVoucherCode ] [] ]
        ]


redeemBtn : Html OnInput
redeemBtn =
    div [ class "form-group row" ]
        [ div [ class "col-sm-offset-2 col-sm-6" ] [ button [ class "btn btn-default", type_ "submit" ] [ text "Redeem" ] ]
        ]


toFormData : Values -> FormData
toFormData values =
    [ ( "username", values.username )
    , ( "password", values.password )
    , ( "partnerVoucherCode", values.partnerVoucherCode )
    ]
