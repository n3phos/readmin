module Promotions.EditForm exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, onClick, onSubmit)
import Promotions.Model exposing (..)
import Json.Decode as JD exposing (..)
import String.Extra exposing (humanize)
import Hotels.Model exposing (..)
import Json.Encode as JE exposing (..)
import Dict exposing (..)
import Vouchers.Model exposing (Credentials)


type Msg
    = OnInput


type alias State =
    { promotion : Promotion
    , hotels : List Hotel
    , hotelsById : Dict String String
    }



{- }
   initState : Promotion -> List Hotel -> State
   initState promotion hotels =
       let
           promotion =
                       PM.getPromotionById pmstate.promotionsById id

                   hotels =
                       pmstate.hotels

           byId =
               List.map sortHotelsById hotels

           hotelsById =
               Dict.fromList byId
       in
           State promotion hotels hotelsById
-}


newState : State
newState =
    State (Promotion "" "" "") [] Dict.empty


type OnInput
    = Save
    | OnInputHotel String
    | OnInputDays String


type Configuration msg
    = Configuration
        { toMsg : OnInput -> msg
        }


stateFromModel : PromotionRaw -> List Hotel -> Dict String String -> State
stateFromModel rawPromotion hotels hotelsDict =
    let
        hotelName =
            Dict.get rawPromotion.hotelId hotelsDict

        name =
            case hotelName of
                Just n ->
                    n

                Nothing ->
                    "Not Set"

        id =
            rawPromotion.docId

        days =
            rawPromotion.days

        promotion =
            Promotion name days id
    in
        State promotion hotels hotelsDict


sortHotelsById : Hotel -> ( String, String )
sortHotelsById hotel =
    ( hotel.id, hotel.title )


getPromotionById : Dict String Promotion -> PromotionId -> Promotion
getPromotionById lookup promotionId =
    let
        maybePromotion =
            Dict.get promotionId lookup

        promotion =
            case maybePromotion of
                Just p ->
                    p

                Nothing ->
                    Promotion "" "" ""
    in
        promotion


view : Configuration msg -> State -> Html msg
view (Configuration { toMsg }) state =
    Html.map toMsg <|
        Html.form
            [ style [ ( "margin-bottom", "5rem" ) ], onSubmit Save ]
            (promotionToHtml state.hotels state.promotion)


update : (JE.Value -> Credentials -> Cmd msg) -> OnInput -> State -> ( State, Cmd msg )
update submitCmd msg state =
    let
        { promotion, hotels, hotelsById } =
            state
    in
        case msg of
            OnInputHotel x ->
                ( updatePromotion { promotion | name = x } state, Cmd.none )

            OnInputDays x ->
                ( updatePromotion { promotion | days = x } state, Cmd.none )

            Save ->
                ( state, applySubmitToValues submitCmd state )


updatePromotion : Promotion -> State -> State
updatePromotion promo state =
    { state | promotion = promo }


hotelSelect : List Hotel -> String -> Html OnInput
hotelSelect hotels currentValue =
    wrapControl
        "hotel"
        select
        [ on "change" <| selectStringMessageDecoder OnInputHotel ]
        (hotelOptions hotels currentValue)


daysInput : String -> Html OnInput
daysInput currentValue =
    div [ class "form-group row" ]
        [ label [ class "col-sm-2 control-label" ] [ text "Number of days" ]
        , div [ class "col-sm-6" ] [ input [ class "form-control", placeholder "Enter number of days", Html.Attributes.value currentValue, required True, type_ "text", onInput OnInputDays ] [] ]
        ]


promotionToHtml : List Hotel -> Promotion -> List (Html OnInput)
promotionToHtml hotels promotion =
    [ hotelSelect hotels promotion.name
    , daysInput promotion.days
    , saveBtn
    ]


saveBtn : Html OnInput
saveBtn =
    button [ class "btn btn-success", type_ "submit" ] [ text "Save" ]


applySubmitToValues : (JE.Value -> Credentials -> Cmd msg) -> State -> Cmd msg
applySubmitToValues submitCmd state =
    let
        promotion =
            state.promotion

        id =
            Dict.get promotion.name state.hotelsById

        json =
            JE.object
                [ ( "username", JE.string "deymold" )
                , ( "password", JE.string "xiagh0y3" )
                , ( "hotelId", JE.string "7" )
                , ( "days", JE.string promotion.days )
                ]
    in
        submitCmd json (Credentials "deymold" "xiagh0y3")


selectStringMessageDecoder : (String -> msg) -> JD.Decoder msg
selectStringMessageDecoder toMsg =
    let
        targetValue : JD.Decoder String
        targetValue =
            JD.at [ "target", "value" ] JD.string
    in
        JD.map toMsg targetValue


inputId : String -> String
inputId inputName =
    "input-" ++ inputName


wrapControl :
    String
    -> (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
wrapControl controlName controlFactory extraAttributes children =
    div [ class "form-group" ]
        [ label [ for <| inputId controlName ] [ text <| humanize controlName ]
        , controlFactory
            ([ id <| inputId controlName, class "form-control", required True ] ++ extraAttributes)
            children
        ]


hotelOptions : List Hotel -> String -> List (Html msg)
hotelOptions hotels currentValue =
    option
        [ Html.Attributes.value "nope"
        , hidden True
        , disabled True
        , selected (currentValue == "")
        ]
        [ text "(please select)" ]
        :: List.map
            (\h ->
                option
                    [ Html.Attributes.value h.title
                    , selected ((h.title) == currentValue)
                    ]
                    [ text h.title ]
            )
            hotels
