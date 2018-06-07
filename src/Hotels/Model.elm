module Hotels.Model exposing (..)

import Json.Decode as JD exposing (..)


type alias HotelId =
    String


type alias HotelName =
    String


type alias Hotel =
    { id : HotelId
    , title : HotelName
    }


recordDecoder : JD.Decoder Hotel
recordDecoder =
    JD.map2
        Hotel
        (JD.field "id" string)
        (JD.field "name" string)


recordRowsDecoder : JD.Decoder (List Hotel)
recordRowsDecoder =
    (JD.list recordDecoder)


sortHotelsById : Hotel -> ( String, String )
sortHotelsById hotel =
    ( hotel.id, hotel.title )
