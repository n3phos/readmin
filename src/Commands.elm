module Commands
    exposing
        ( initCommand
        , routeCommand
        , modelOnRouteChange
        , postAdvertisement
        , putAdvertisement
        , deleteAdvertisement
        , postVoucher
        , requestDateCommand
        , redeemVoucher
        , postPromotion
        , putPromotion
        , deletePromotion
        )

import Http
import Date
import Task exposing (..)
import Platform.Cmd exposing (batch)
import Json.Encode as JE
import Json.Decode as JD
import RemoteData
import Msgs exposing (Msg)
import BookingStats exposing (recordRowsDecoder)
import Advertisements exposing (advertisementRowsDecoder, advertisementRowDecoder)
import Advertisements.Model
    exposing
        ( AdvertisementId
        , AdvertisementIdKey(..)
        , AdvertisementIdRevision(..)
        )
import Locale exposing (localeRowsDecoder)
import Host exposing (domainByKeyMapDecoder)
import Model exposing (Model)
import Routing exposing (Route(..))
import Advertisements.Form as AF
import BasicAuth exposing (..)
import Exts.Http exposing (..)
import Vouchers.Model exposing (..)
import Hotels.Model exposing (..)
import Promotions.Model exposing (..)


--import Promotions.Model exposing (..)


initCommand : Route -> Cmd Msg
initCommand route =
    batch [ fetchHotels, fetchVouchers (Credentials "deymold" "xiagh0y3"), requestDateCommand, determineToday, fetchStats, fetchLocales, fetchHosts, routeCommand route ]


determineToday : Cmd Msg
determineToday =
    Task.perform Msgs.OnSetToday Date.now


modelOnRouteChange : Route -> Model -> Model
modelOnRouteChange newRoute model =
    let
        modelWithNewRoute =
            { model | route = newRoute }
    in
        (case newRoute of
            AdvertisementsRoute ->
                { modelWithNewRoute | advertisements = RemoteData.Loading }

            AdvertisementRoute _ ->
                { modelWithNewRoute | currentAdvertisementId = RemoteData.Loading }

            NewAdvertisementRoute ->
                { modelWithNewRoute
                    | currentAdvertisementId = RemoteData.NotAsked
                    , advertisementFormState = AF.newState
                }

            PromotionsRoute ->
                { modelWithNewRoute | promotions = RemoteData.Loading }

            _ ->
                modelWithNewRoute
        )


routeCommand : Route -> Cmd Msg
routeCommand route =
    case route of
        AdvertisementsRoute ->
            fetchAdvertisements

        AdvertisementRoute idKey ->
            fetchAdvertisement idKey

        PromotionRoute id ->
            fetchPromotion id

        PromotionsRoute ->
            fetchPromotions

        _ ->
            Cmd.none



-- HTTP


fetchStats : Cmd Msg
fetchStats =
    Http.get "/couchdb/booking-orders/_design/stats/_view/by_day?limit=100&descending=true&group=true" BookingStats.recordRowsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchStats


fetchLocales : Cmd Msg
fetchLocales =
    Http.get "/catalog/json/supported-locales.json" localeRowsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchLocales


fetchHosts : Cmd Msg
fetchHosts =
    Http.get "/catalog/json/partner-hosts.json" domainByKeyMapDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchHosts


fetchAdvertisements : Cmd Msg
fetchAdvertisements =
    Http.get "/couchdb/advertisement/_design/main/_view/all" advertisementRowsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchAdvertisements


fetchAdvertisement : AdvertisementIdKey -> Cmd Msg
fetchAdvertisement (AdvertisementIdKey k) =
    Http.get ("/couchdb/advertisement/" ++ k) advertisementRowDecoder
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchAdvertisement


postAdvertisement : JE.Value -> Cmd Msg
postAdvertisement advertisementDataValue =
    Http.post "/couchdb/advertisement" (Http.jsonBody advertisementDataValue) (JD.succeed "")
        |> Http.send Msgs.AdvertisementCreated


putAdvertisement : AdvertisementId -> JE.Value -> Cmd Msg
putAdvertisement adId advertisementDataValue =
    Http.request
        { method = "PUT"
        , headers = []
        , url = advertisementVersionUrl adId
        , body = Http.jsonBody advertisementDataValue
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send Msgs.AdvertisementUpdated


deleteAdvertisement : AdvertisementId -> Cmd Msg
deleteAdvertisement adId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = advertisementVersionUrl adId
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send Msgs.AdvertisementDeleted


advertisementVersionUrl : AdvertisementId -> String
advertisementVersionUrl { key, revision } =
    let
        (AdvertisementIdKey k) =
            key

        (AdvertisementIdRevision r) =
            revision
    in
        "/couchdb/advertisement/" ++ k ++ "?rev=" ++ r


promotionUrl : PromotionId -> String
promotionUrl id =
    couchDBUrl "/promotions/" ++ id


requestToService : Url -> FormData -> Credentials -> Cmd Msg
requestToService url formData { username, password } =
    Http.request
        { method = "POST"
        , headers = [ buildAuthorizationHeader username password ]
        , url = voucherServiceUrl url
        , body = Exts.Http.formBody formData
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send Msgs.OnVoucherGenerated


postVoucher : FormData -> Credentials -> Cmd Msg
postVoucher formData creds =
    requestToService "services/code/gen" formData creds


redeemVoucher : FormData -> Credentials -> Cmd Msg
redeemVoucher formData creds =
    requestToService "services/code/redeem" formData creds


voucherServiceUrl : String -> String
voucherServiceUrl path =
    "https://cumulus-api.shbox.local/" ++ path


couchDBUrl : String -> String
couchDBUrl path =
    "http://couchdb.shbox.local" ++ path


catalogUrl : String -> String
catalogUrl path =
    "http://swisshotels.shbox.local/catalog/json/" ++ path


requestDateCommand : Cmd Msg
requestDateCommand =
    Task.perform Msgs.ReceiveDate Date.now


fetchVouchers : Credentials -> Cmd Msg
fetchVouchers { username, password } =
    Http.request
        { method = "GET"
        , headers = [ buildAuthorizationHeader username password ]
        , url = "/couchdb/cumulus-extra-vouchers/_design/admin/_view/bookable_by_timestamp"
        , body = Http.emptyBody
        , expect = Http.expectJson Vouchers.Model.recordRowsDecoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send Msgs.OnFetchVouchers


fetchHotelsOld : Credentials -> Http.Request (List Hotel)
fetchHotelsOld { username, password } =
    Http.request
        { method = "GET"
        , headers = [ buildAuthorizationHeader username password ]
        , url = catalogUrl "all-hotel-names.json"
        , body = Http.emptyBody
        , expect = Http.expectJson Hotels.Model.recordRowsDecoder
        , timeout = Nothing
        , withCredentials = False
        }


fetchHotels : Cmd Msg
fetchHotels =
    Http.get "/catalog/json/all-hotel-names.json" (Hotels.Model.recordRowsDecoder)
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchHotels


fetchPromotions : Cmd Msg
fetchPromotions =
    Http.get "/couchdb/promotions/_design/main/_view/all" (Promotions.Model.recordRowsDecoder)
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchPromotions


fetchPromotion : PromotionId -> Cmd Msg
fetchPromotion id =
    Http.get (promotionUrl id) (Promotions.Model.recordDecoder)
        |> RemoteData.sendRequest
        |> Cmd.map Msgs.OnFetchPromotion


postPromotion : JE.Value -> Cmd Msg
postPromotion json =
    Http.post "/couchdb/promotions" (Http.jsonBody json) (JD.succeed "")
        |> Http.send Msgs.PromotionCreated


putPromotion : JE.Value -> Credentials -> Cmd Msg
putPromotion json { username, password } =
    Http.request
        { method = "PUT"
        , headers = [ buildAuthorizationHeader username password ]
        , url = "/couchdb/promotions"
        , body = Http.jsonBody json
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send Msgs.PromotionUpdated

deletePromotion: PromotionId -> Cmd Msg
deletePromotion promotionId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/couchdb/promotions" ++ promotionId
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send Msgs.PromotionDeleted
