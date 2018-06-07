module Msgs exposing (..)

import Date exposing (Date)
import Http
import RemoteData exposing (WebData)
import Navigation exposing (Location)
import BookingStats exposing (BookingStatsRecord)
import Advertisements.Model exposing (Advertisement, AdvertisementId)
import Advertisements.Form as AF
import Locale exposing (LocaleCode)
import Host exposing (Host)
import Keyboard exposing (KeyCode)
import Vouchers.GenerateForm as GF
import Vouchers.RedeemForm as RF
import Promotions as PM
import Vouchers.Model exposing (Voucher)
import Hotels.Model exposing (Hotel)
import Promotions.Model exposing (..)
import Promotions.EditForm as EF


type Msg
    = OnSetToday Date
    | OnFetchStats (WebData (List BookingStatsRecord))
    | OnFetchLocales (WebData (List LocaleCode))
    | OnFetchHosts (WebData (List Host))
    | OnLocationChange Location
    | OnFetchAdvertisements (WebData (List Advertisement))
    | OnFetchAdvertisement (WebData Advertisement)
    | ChangeAdvertisementForm AF.OnInput
    | AdvertisementCreated (Result Http.Error String)
    | AdvertisementUpdated (Result Http.Error String)
    | DeleteAdvertisementAsk AdvertisementId String
    | DeleteAdvertisement AdvertisementId
    | AdvertisementDeleted (Result Http.Error String)
    | CloseModalConfirmation
    | KeyUp KeyCode
    | OnVoucherGenerated (Result Http.Error String)
    | GenerateVoucher
    | ChangeGenerateVoucherForm GF.OnInput
    | ReceiveDate Date
    | ChangeRedeemVoucherForm RF.OnInput
    | OnFetchVouchers (Result Http.Error (List Voucher))
    | OnFetchHotels (WebData (List Hotel))
    | OnFetchPromotions (WebData (List PromotionRaw))
    | OnPromotionAndHotelFetch (Result Http.Error Promos)
    | OnPromotionEvent PM.PromoMsg
    | PromotionCreated (Result Http.Error String)
    | ChangePromotionForm EF.OnInput
    | PromotionUpdated (Result Http.Error String)
    | OnFetchPromotion (WebData PromotionRaw)
    | PromotionDeleted (Result Http.Error String)
