module Promotions.Model exposing (..)

import Json.Decode as JD exposing (..)
import Hotels.Model exposing (Hotel)


type alias PromotionRaw =
    { hotelId : String
    , days : String
    , docId : String
    }


type alias Promotion =
    { name : String
    , days : String
    , id : String
    }


type alias Promos =
    { promotionsRaw : List PromotionRaw
    , hotels : List Hotel
    }


type alias PromotionId =
    String


recordDecoder : JD.Decoder PromotionRaw
recordDecoder =
    JD.map3
        PromotionRaw
        (JD.at [ "value", "hotelId" ] nullToNotSetDecoder)
        (JD.at [ "value", "days" ] nullToNotSetDecoder)
        (JD.field "id" string)


nullToNotSetDecoder : JD.Decoder String
nullToNotSetDecoder =
    oneOf [ string, null "Not Set" ]


recordRowsDecoder : JD.Decoder (List PromotionRaw)
recordRowsDecoder =
    JD.field "rows" (JD.list recordDecoder)
