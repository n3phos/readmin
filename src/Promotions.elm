module Promotions exposing (..)

import Promotions.Model exposing (..)
import Hotels.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, onClick, onSubmit)
import Dict exposing (..)
import Vouchers.Model exposing (..)
import Json.Encode as JE


type alias State =
    { promotionsRaw : List PromotionRaw
    , hotels : List Hotel
    , dict : Dict String String
    , promotions : List Promotion
    , promotionsById : Dict String Promotion
    }


newState : State
newState =
    State [] [] Dict.empty [] Dict.empty


type Msg
    = PromoMsg


type PromoMsg
    = InitPromotions Promos
    | AddPromotion
    | DeletePromotion String


type Configuration msg
    = Configuration
        { toMsg : PromoMsg -> msg
        }


view : Configuration msg -> State -> Html msg
view (Configuration { toMsg }) state =
    Html.map toMsg <|
        div []
            [ h2 [] [ text "Promotions" ]
            , promotionsTable state
            , addPromotionBtn
            ]


update : (JE.Value -> Cmd msg) -> (PromotionId -> Cmd msg) -> PromoMsg -> State -> ( State, Cmd msg )
update addPromotionCmd deletePromotionCmd msg state =
    case msg of
        InitPromotions promos ->
            ( (initPromotions promos state), Cmd.none )

        AddPromotion ->
            ( state, applyAddPromotionToValues addPromotionCmd )

        DeletePromotion id ->
            ( state, (deletePromotionCmd id) )


promotionToHtml : Promotion -> Html PromoMsg
promotionToHtml promotion =
    tr []
        [ td [] [ text (promotion.name) ]
        , td [] [ text (promotion.days) ]
        , td [] [ a [ class "btn btn-primary", href (promotionRoute promotion.id) ] [ text "Edit" ], button [ class "btn btn-danger", onClick (DeletePromotion (promotionRoute promotion.id)) ] [ text "Delete" ] ]
        ]


initPromotions : Promos -> State -> State
initPromotions promos state =
    let
        dictKeyValues =
            List.map getIdAndName promos.hotels

        dict =
            Dict.fromList dictKeyValues

        promotions =
            List.map (createPromotions dict) promos.promotionsRaw

        newstate =
            { state | promotionsRaw = promos.promotionsRaw, hotels = promos.hotels, dict = dict, promotions = promotions }
    in
        (newstate)


getIdAndName : Hotel -> ( String, String )
getIdAndName hotel =
    ( hotel.id, hotel.title )


createPromotions : Dict String String -> PromotionRaw -> Promotion
createPromotions dict { hotelId, days, docId } =
    let
        hotel =
            Dict.get hotelId dict

        name =
            case hotel of
                Just n ->
                    n

                Nothing ->
                    "Not Set"
    in
        ({ name = name, days = days, id = docId })


promotionsTable : State -> Html PromoMsg
promotionsTable state =
    table [ class "table table-bordered table-hover table-condensed" ]
        [ thead []
            [ tr [ style [ ( "font-weight", "bold" ) ] ]
                [ td [ style [ ( "width", "20%" ) ] ] [ text "Hotel" ]
                , td [ style [ ( "width", "40%" ) ] ] [ text "Number of days" ]
                ]
            ]
        , tbody [] (List.map promotionToHtml state.promotions)
        ]


addPromotionBtn : Html PromoMsg
addPromotionBtn =
    button [ class "btn btn-success", onClick AddPromotion ] [ span [ class "fa fa-plus" ] [], text "Add promotion" ]


applyAddPromotionToValues : (JE.Value -> Cmd msg) -> Cmd msg
applyAddPromotionToValues addPromotionCmd =
    let
        json =
            JE.object
                [ ( "hotelId", JE.string "" )
                , ( "days", JE.string "" )
                ]
    in
        addPromotionCmd json


promotionRoute : PromotionId -> String
promotionRoute id =
    "#promotions/" ++ id
