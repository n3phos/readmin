module Model exposing (Model, LastError(..))

import Date exposing (Date)
import Http exposing (Error)
import RemoteData exposing (WebData)
import BookingStats exposing (BookingStatsRecord)
import Advertisements.Model exposing (Advertisement, AdvertisementId)
import Advertisements.Form as AF
import Vouchers.GenerateForm as GF
import Vouchers.RedeemForm as RF
import Vouchers.Table as VT
import Promotions as PM
import Promotions.EditForm as EF
import Host exposing (Host)
import Locale exposing (LocaleCode)
import Routing exposing (Route)
import ModalConfirmation exposing (ModalConfirmation)
import Promotions.Model exposing (PromotionRaw)
import Hotels.Model exposing (Hotel)
import Dict exposing (..)


type alias Model =
    { today : Date
    , stats : WebData (List BookingStatsRecord)
    , locales : WebData (List LocaleCode)
    , hosts : WebData (List Host)
    , route : Route
    , hotels : List Hotel
    , hotelsById : Dict String String
    , advertisements : WebData (List Advertisement)
    , advertisementFormState : AF.State
    , currentAdvertisementId : WebData AdvertisementId
    , generateVoucherFormState : GF.State
    , redeemVoucherFormState : RF.State
    , voucherTableState : VT.State
    , promotionsState : PM.State
    , promotions : WebData (List PromotionRaw)
    , editPromotionState : EF.State
    , lastError : LastError
    , modalConfirmation : ModalConfirmation
    }


type LastError
    = NoErrorsYet
    | HttpError Error
