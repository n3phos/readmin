module Vouchers.Model exposing (..)

import Json.Decode as JD exposing (..)
import DateDecoder exposing (dateTimeDecoder)


type alias VoucherData =
    { username : String
    , password : String
    , partnerOfferId : String
    , voucherCode : String
    }


type alias Voucher =
    { dateTime : String
    , partnerVoucherCode : String
    , partnerOfferId : Maybe String
    }


recordDecoder : JD.Decoder Voucher
recordDecoder =
    JD.map3
        Voucher
        (JD.field "key" dateTimeDecoder)
        (JD.at [ "value" ] (JD.index 0 string))
        (JD.at [ "value" ] (JD.index 1 (maybe string)))


recordRowsDecoder : JD.Decoder (List Voucher)
recordRowsDecoder =
    JD.field "rows" (JD.list recordDecoder)


type alias Url =
    String


type alias Username =
    String


type alias Password =
    String


type alias FormData =
    List ( String, String )


type alias Credentials =
    { username : String
    , password : String
    }
