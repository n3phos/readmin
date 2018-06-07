module Main exposing (..)

import Date exposing (Date)
import Navigation exposing (Location, program, newUrl)
import Http
import Json.Encode as JE
import RemoteData exposing (WebData)
import Routing exposing (parseLocation, Route(BookingStatsRoute))
import Model exposing (Model, LastError(..))
import Msgs exposing (Msg)
import Commands
    exposing
        ( initCommand
        , routeCommand
        , modelOnRouteChange
        , postAdvertisement
        , putAdvertisement
        , deleteAdvertisement
        , postVoucher
        , redeemVoucher
        , requestDateCommand
        , postPromotion
        , putPromotion
        , deletePromotion
        )
import View exposing (view)
import Advertisements.Form as AF
import Vouchers.GenerateForm as GF
import Vouchers.RedeemForm as RF
import Vouchers.Table as VT
import Advertisements.Model exposing (Advertisement, AdvertisementId)
import ModalConfirmation exposing (ModalConfirmation(..))
import Keyboard exposing (KeyCode)
import Vouchers.Model exposing (..)
import Promotions as PM
import Promotions.Model exposing (..)
import Promotions.EditForm as EF
import Hotels.Model exposing (..)
import Dict exposing (..)


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            parseLocation location
    in
        ( initialModel currentRoute, initCommand currentRoute )


initialModel : Route -> Model
initialModel route =
    { today = Date.fromTime 0
    , stats = RemoteData.Loading
    , locales = RemoteData.Loading
    , hosts = RemoteData.Loading
    , hotels = []
    , route = route
    , advertisements = RemoteData.NotAsked
    , advertisementFormState = AF.newState
    , generateVoucherFormState = GF.newState
    , redeemVoucherFormState = RF.newState
    , voucherTableState = VT.newState
    , promotionsState = PM.newState
    , editPromotionState = EF.newState
    , hotelsById = Dict.empty
    , promotions = RemoteData.NotAsked
    , currentAdvertisementId = RemoteData.NotAsked
    , lastError = NoErrorsYet
    , modalConfirmation = ModalConfirmationNone
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.OnFetchStats response ->
            ( { model | stats = response }, Cmd.none )

        Msgs.OnFetchLocales response ->
            ( { model | locales = response }, Cmd.none )

        Msgs.OnFetchHosts response ->
            ( { model | hosts = response }, Cmd.none )

        Msgs.OnSetToday today ->
            ( { model | today = today }, Cmd.none )

        Msgs.OnLocationChange location ->
            onLocationChange location model

        Msgs.OnFetchAdvertisements response ->
            ( { model | advertisements = response }, Cmd.none )

        Msgs.OnFetchAdvertisement response ->
            onFetchAdvertisement response model

        Msgs.ChangeAdvertisementForm formMsg ->
            changeAdvertisementForm formMsg model

        Msgs.AdvertisementCreated result ->
            onResultOfAdMutation result model

        Msgs.AdvertisementUpdated result ->
            onResultOfAdMutation result <| clearTheEditedAdvertisement model

        Msgs.DeleteAdvertisementAsk adId title ->
            ( { model | modalConfirmation = ModalConfirmationDeleteAdvertisement adId title }
            , Cmd.none
            )

        Msgs.DeleteAdvertisement id ->
            ( { model | modalConfirmation = ModalConfirmationNone }, deleteAdvertisement id )

        Msgs.AdvertisementDeleted result ->
            onResultOfAdMutation result model

        Msgs.CloseModalConfirmation ->
            ( { model | modalConfirmation = ModalConfirmationNone }, Cmd.none )

        Msgs.KeyUp code ->
            keyPress code model

        Msgs.GenerateVoucher ->
            ( model, Cmd.none )

        Msgs.ChangeGenerateVoucherForm formMsg ->
            changeGenerateVoucherForm formMsg model

        Msgs.ChangeRedeemVoucherForm formMsg ->
            changeRedeemVoucherForm formMsg model

        Msgs.ReceiveDate x ->
            changeGenerateVoucherForm (GF.OnReceiveDate x) model

        Msgs.OnVoucherGenerated response ->
            onVoucherGenerated response model

        Msgs.OnFetchVouchers response ->
            onFetchVouchers response model

        Msgs.OnFetchHotels response ->
            onFetchHotels response model

        Msgs.OnFetchPromotions response ->
           onFetchPromotions response model

        Msgs.OnPromotionAndHotelFetch response ->
            onPromotionAndHotelFetch response model

        Msgs.OnPromotionEvent msg ->
            onPromotionEvent msg model

        Msgs.PromotionCreated response ->
           onResultOfPromoMutation response model

        Msgs.PromotionDeleted response ->
            onResultOfPromoMutation response model

        Msgs.ChangePromotionForm msg ->
            changePromotionForm msg model

        Msgs.PromotionUpdated msg ->
            ( model, Cmd.none )

        Msgs.OnFetchPromotion response ->
            onFetchPromotion response model


subscriptions : Model -> Sub Msg
subscriptions model =
    (Keyboard.ups Msgs.KeyUp)


main : Program Never Model Msg
main =
    program Msgs.OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


onLocationChange : Location -> Model -> ( Model, Cmd Msg )
onLocationChange location model =
    let
        newRoute =
            parseLocation location
    in
        ( modelOnRouteChange newRoute model, routeCommand newRoute )


onFetchAdvertisement : WebData Advertisement -> Model -> ( Model, Cmd msg )
onFetchAdvertisement response model =
    ( { model
        | currentAdvertisementId = RemoteData.map .id response
        , advertisementFormState =
            case response of
                RemoteData.Success ad ->
                    AF.stateFromModel ad.data

                _ ->
                    AF.newState
      }
    , Cmd.none
    )


onFetchPromotion : WebData PromotionRaw -> Model -> ( Model, Cmd msg )
onFetchPromotion response model =
    ( { model
        | editPromotionState =
            case response of
                RemoteData.Success promotion ->
                    EF.stateFromModel promotion model.hotels model.hotelsById

                _ ->
                    EF.newState
      }
    , Cmd.none
    )


onFetchHotels : WebData (List Hotel) -> Model -> ( Model, Cmd msg )
onFetchHotels response model =
    case response of
        RemoteData.Success hotels ->
            let
                sorted =
                    List.map sortHotelsById hotels

                hotelsDict =
                    Dict.fromList sorted
            in
                ( { model | hotels = hotels, hotelsById = hotelsDict }, Cmd.none )

        RemoteData.NotAsked ->
            ( model, Cmd.none )

        RemoteData.Loading ->
            ( model, Cmd.none )

        RemoteData.Failure error ->
            ( { model | lastError = HttpError error }, newUrl "#last-error" )


changeAdvertisementForm : AF.OnInput -> Model -> ( Model, Cmd Msg )
changeAdvertisementForm formMsg model =
    let
        ( state, cmd ) =
            AF.update
                (advertisementSubmitCommand model.currentAdvertisementId)
                formMsg
                model.advertisementFormState
    in
        ( { model | advertisementFormState = state }
        , cmd
        )


changeGenerateVoucherForm : GF.OnInput -> Model -> ( Model, Cmd Msg )
changeGenerateVoucherForm formMsg model =
    let
        ( state, cmd ) =
            GF.update
                (postVoucher)
                requestDateCommand
                formMsg
                model.generateVoucherFormState
    in
        ( { model | generateVoucherFormState = state }, cmd )


changePromotionForm : EF.OnInput -> Model -> ( Model, Cmd Msg )
changePromotionForm formMsg model =
    let
        ( state, cmd ) =
            EF.update
                (putPromotion)
                formMsg
                model.editPromotionState
    in
        ( { model | editPromotionState = state }, cmd )


changeRedeemVoucherForm : RF.OnInput -> Model -> ( Model, Cmd Msg )
changeRedeemVoucherForm formMsg model =
    let
        ( state, cmd ) =
            RF.update
                (redeemVoucher)
                formMsg
                model.redeemVoucherFormState
    in
        ( { model | redeemVoucherFormState = state }, cmd )


populateVoucherTable : List Voucher -> Model -> ( Model, Cmd Msg )
populateVoucherTable list model =
    let
        ( state, cmd ) =
            VT.update
                list
                model.voucherTableState
    in
        ( { model | voucherTableState = state }, cmd )


onResultOfAdMutation : Result Http.Error String -> Model -> ( Model, Cmd Msg )
onResultOfAdMutation result model =
    case result of
        Err error ->
            ( { model | lastError = HttpError error }, newUrl "#last-error" )

        Ok _ ->
            ( model, newUrl "#advertisements" )


onVoucherGenerated : Result Http.Error String -> Model -> ( Model, Cmd Msg )
onVoucherGenerated result model =
    case result of
        Ok message ->
            changeGenerateVoucherForm (GF.OnVoucherGenerated message) model

        Err error ->
            ( model, Cmd.none )


onFetchVouchers : Result Http.Error (List Voucher) -> Model -> ( Model, Cmd Msg )
onFetchVouchers result model =
    case result of
        Ok list ->
            populateVoucherTable list model

        Err error ->
            ( { model | lastError = HttpError error }, newUrl "#last-error" )

onResultOfPromoMutation : Result Http.Error String -> Model -> ( Model, Cmd Msg )
onResultOfPromoMutation result model =
    case result of
        Err error ->
            ( { model | lastError = HttpError error }, newUrl "#last-error" )

        Ok _ ->
            ( model, newUrl "#promotions" )



onFetchPromotions : RemoteData.WebData (List PromotionRaw) -> Model -> ( Model, Cmd Msg )
onFetchPromotions response model =
   case response of
       RemoteData.Success rawPromotions ->
         let
             promos = Promos rawPromotions model.hotels
         in
             onPromotionEvent (PM.InitPromotions promos) model

       RemoteData.Failure error ->
           ( { model | lastError = HttpError error }, newUrl "#last-error" )

       RemoteData.Loading ->
            (model, Cmd.none)

       RemoteData.NotAsked ->
            (model, Cmd.none)



onPromotionAndHotelFetch : Result Http.Error Promos -> Model -> ( Model, Cmd Msg )
onPromotionAndHotelFetch result model =
    case result of
        Ok promos ->
            onPromotionEvent (PM.InitPromotions promos) model

        Err error ->
            ( { model | lastError = HttpError error }, newUrl "#last-error" )


onPromotionEvent : PM.PromoMsg -> Model -> ( Model, Cmd Msg )
onPromotionEvent promomsg model =
    let
        ( state, cmd ) =
            PM.update postPromotion deletePromotion promomsg model.promotionsState
    in
        ( { model | promotionsState = state }, cmd )


keyPress : KeyCode -> Model -> ( Model, Cmd Msg )
keyPress code model =
    case ( code, model.modalConfirmation ) of
        ( 13, ModalConfirmationDeleteAdvertisement adId _ ) ->
            ( { model | modalConfirmation = ModalConfirmationNone }, deleteAdvertisement adId )

        ( 27, _ ) ->
            ( { model | modalConfirmation = ModalConfirmationNone }, Cmd.none )

        _ ->
            ( model, Cmd.none )


clearTheEditedAdvertisement : Model -> Model
clearTheEditedAdvertisement model =
    { model | currentAdvertisementId = RemoteData.NotAsked }


advertisementSubmitCommand : WebData AdvertisementId -> (JE.Value -> Cmd Msg)
advertisementSubmitCommand currentAdvertisementId =
    case currentAdvertisementId of
        RemoteData.Success adId ->
            putAdvertisement adId

        RemoteData.NotAsked ->
            postAdvertisement

        _ ->
            \_ -> Cmd.none
