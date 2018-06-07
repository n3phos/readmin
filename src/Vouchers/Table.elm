module Vouchers.Table exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Vouchers.Model exposing (..)


type alias State =
    { values : Values }


newState : State
newState =
    State (Values ([]))


type alias Values =
    { vouchers : List Voucher
    }


view : State -> Html msg
view state =
    div []
        [ h2 [] [ text "Voucher table" ]
        , table [ class "table table-bordered table-hover table-condensed" ]
            [ thead []
                [ tr [ style [ ( "font-weight", "bold" ) ] ]
                    [ td [ style [ ( "width", "30%" ) ] ] [ text "Datetime" ]
                    , td [ style [ ( "width", "40%" ) ] ] [ text "Partner voucher code" ]
                    , td [ style [ ( "width", "30%" ) ] ] [ text "Partner offer id" ]
                    ]
                ]
            , tbody [] (List.map voucherToHtml state.values.vouchers)
            ]
        ]


update : List Voucher -> State -> ( State, Cmd msg )
update list state =
    let
        { values } =
            state
    in
        ( updateValues { values | vouchers = list } state, Cmd.none )


updateValues : Values -> State -> State
updateValues values state =
    { state | values = values }


voucherToHtml : Voucher -> Html msg
voucherToHtml voucher =
    tr []
        [ td [] [ text (voucher.dateTime) ]
        , td [] [ text (voucher.partnerVoucherCode) ]
        , td [] [ text (getPartnerOfferId (voucher.partnerOfferId)) ]
        ]


getPartnerOfferId : Maybe String -> String
getPartnerOfferId maybe =
    case maybe of
        Nothing ->
            "not a real string"

        Just partnerOfferId ->
            partnerOfferId
