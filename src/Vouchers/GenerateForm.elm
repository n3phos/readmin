module Vouchers.GenerateForm exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, onClick, onSubmit)
import Date exposing (Date)
import Json.Encode as JE
import FormatDate exposing (..)
import Vouchers.Model exposing (..)


type alias State =
    { values : Values }


newState : State
newState =
    State
        (Values "" "hello" "" "" (Date.fromTime (0)) "")


type alias Values =
    { username : String
    , password : String
    , partnerOfferId : String
    , voucherCode : String
    , date : Date
    , resultLog : String
    }


type Msg
    = OnInput


type OnInput
    = OnInputUsername String
    | OnInputPassword String
    | OnInputPartnerOfferId String
    | OnInputVoucherCode String
    | RequestDate
    | OnReceiveDate Date
    | Submit
    | OnVoucherGenerated String


type Configuration msg
    = Configuration
        { toMsg : OnInput -> msg
        }


view : Configuration msg -> State -> Html msg
view (Configuration { toMsg }) state =
    Html.map toMsg <|
        Html.form
            [ style [ ( "margin-bottom", "5rem" ) ], onSubmit Submit ]
            ([ resultLog state
             , usernameInput
             , passwordInput
             , partnerOfferIdInput
             , voucherCodeInput state
             , generateBtn
             ]
            )


update : (FormData -> Credentials -> Cmd msg) -> Cmd msg -> OnInput -> State -> ( State, Cmd msg )
update submitCmd requestDateCommand msg state =
    let
        { values } =
            state
    in
        case msg of
            OnInputUsername x ->
                ( updateValues { values | username = x } state, Cmd.none )

            OnInputPassword x ->
                ( updateValues { values | password = x } state, Cmd.none )

            OnInputPartnerOfferId x ->
                ( updateValues { values | partnerOfferId = x } state, Cmd.none )

            OnInputVoucherCode x ->
                ( updateValues { values | voucherCode = x } state, Cmd.none )

            RequestDate ->
                ( state, requestDateCommand )

            OnReceiveDate x ->
                ( updateValues { values | date = x, voucherCode = (generateVoucherCode x) } state, Cmd.none )

            OnVoucherGenerated x ->
                ( updateValues { values | resultLog = x } state, Cmd.none )

            Submit ->
                ( state, applySubmitToValues submitCmd state )


updateValues : Values -> State -> State
updateValues values state =
    { state | values = values }


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


partnerOfferIdInput : Html OnInput
partnerOfferIdInput =
    div [ class "form-group row" ]
        [ label [ class "col-sm-2 control-label" ] [ text "Partner offer id" ]
        , div [ class "col-sm-6" ] [ input [ class "form-control", placeholder "Enter a partner offer id", required True, type_ "text", onInput OnInputPartnerOfferId ] [] ]
        ]


voucherCodeInput : State -> Html OnInput
voucherCodeInput state =
    div [ class "form-group row" ]
        [ label [ class "col-sm-2 control-label" ] [ text "Voucher code" ]
        , div [ class "col-sm-6" ]
            [ div [ class "input-group" ]
                [ button [ class "btn", type_ "button", onClick RequestDate ]
                    [ span [ class "fa fa-refresh" ] [] ]
                , input [ class "form-control", placeholder "Enter a voucher code", type_ "text", value (generateVoucherCode state.values.date) ] []
                ]
            ]
        ]


resultLog : State -> Html OnInput
resultLog state =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ div [ class "panel panel-default" ]
                [ div [ class "panel-heading" ]
                    [ text "Result log"
                    ]
                , div [ class "panel-body" ]
                    [ text "response from swisshotels.com"
                    , text (state.values.resultLog)
                    ]
                ]
            ]
        ]


generateBtn : Html OnInput
generateBtn =
    div [ class "form-group row" ]
        [ div [ class "col-sm-offset-2 col-sm-6" ] [ button [ class "btn btn-default", type_ "submit" ] [ text "Generate" ] ]
        ]


generateVoucherCode : Date -> String
generateVoucherCode date =
    let
        formattedDate =
            toVoucherFormat date
    in
        "GENERATED-" ++ formattedDate


applySubmitToValues : (FormData -> Credentials -> Cmd msg) -> State -> Cmd msg
applySubmitToValues submitCmd { values } =
    let
        voucherDat =
            toVoucherData values

        formData =
            toFormData voucherDat
    in
        submitCmd formData (Credentials values.username values.password)


toFormData : VoucherData -> FormData
toFormData voucherData =
    [ ( "partnerOfferId", voucherData.partnerOfferId )
    , ( "voucherCode", voucherData.voucherCode )
    ]


toJsonValue : VoucherData -> JE.Value
toJsonValue voucherData =
    JE.object
        [ ( "partnerOfferId", JE.string voucherData.partnerOfferId )
        , ( "voucherCode", JE.string voucherData.voucherCode )
        ]


toVoucherData : Values -> VoucherData
toVoucherData values =
    VoucherData
        values.username
        values.password
        values.partnerOfferId
        values.voucherCode
