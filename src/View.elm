module View exposing (view)

import Date exposing (Date)
import Html exposing (Html, text, div, h2)
import Html.Attributes exposing (class)
import RemoteData exposing (WebData)
import Model exposing (Model, LastError(..))
import Navbar exposing (navbar)
import BookingStats as Stats
import Advertisements.Model exposing (Advertisement, AdvertisementIdKey(..))
import Advertisements
import Advertisements.Form as AF
import Vouchers.GenerateForm as GF
import Vouchers.RedeemForm as RF
import Vouchers.Table as VT
import Promotions as PM
import Promotions.EditForm as EF
import Locale exposing (LocaleCode)
import Host exposing (Host)
import Msgs
import Routing exposing (Route(..))
import ModalConfirmation


type alias AdvertisementPageSources =
    { localeData : WebData (List LocaleCode), hostData : WebData (List Host) }


view : Model -> Html Msgs.Msg
view model =
    layout (page model) (ModalConfirmation.view model.modalConfirmation)


layout : Html msg -> Html msg -> Html msg
layout content modal =
    div []
        [ navbar, content, modal ]


page : Model -> Html Msgs.Msg
page model =
    case model.route of
        BookingStatsRoute ->
            pageStats model.today model.stats

        AdvertisementsRoute ->
            pageAdvertisements model.advertisements

        AdvertisementRoute _ ->
            pageEditAdvertisement
                (AdvertisementPageSources model.locales model.hosts)
                model.advertisementFormState

        NewAdvertisementRoute ->
            pageNewAdvertisement
                (AdvertisementPageSources model.locales model.hosts)
                model.advertisementFormState

        GenerateVoucherRoute ->
            pageGenerateVoucher model.generateVoucherFormState model.today

        RedeemVoucherRoute ->
            pageRedeemVoucher model.redeemVoucherFormState

        VoucherTableRoute ->
            pageVoucherTable model.voucherTableState

        PromotionsRoute ->
            pagePromotions model.promotionsState

        PromotionRoute id ->
            pageEditPromotion id model.editPromotionState

        LastErrorRoute ->
            pageLastError model.lastError

        NotFoundRoute ->
            pageNotFound


pageStats : Date -> WebData (List Stats.BookingStatsRecord) -> Html msg
pageStats today stats =
    case stats of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success stats ->
            Stats.view (Stats.range today stats Stats.limitDays)

        RemoteData.Failure error ->
            text (toString error)


pageAdvertisements : WebData (List Advertisement) -> Html Msgs.Msg
pageAdvertisements adsData =
    div [ class "container" ]
        [ h2 [] [ text "Advertisements" ]
        , pageAdvertisementsContent adsData
        ]


pageAdvertisementsContent : WebData (List Advertisement) -> Html Msgs.Msg
pageAdvertisementsContent adsData =
    case adsData of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success ads ->
            Advertisements.view ads

        RemoteData.Failure error ->
            text (toString error)


pageEditAdvertisement : AdvertisementPageSources -> AF.State -> Html Msgs.Msg
pageEditAdvertisement sources advertisementFormState =
    div [ class "container" ]
        [ h2 [] [ text "Edit advertisement" ]
        , pageEditAdvertisementContent sources advertisementFormState
        ]


pageNewAdvertisement : AdvertisementPageSources -> AF.State -> Html Msgs.Msg
pageNewAdvertisement sources advertisementFormState =
    div [ class "container" ]
        [ h2 [] [ text "New advertisement" ]
        , pageEditAdvertisementContent sources advertisementFormState
        ]


pageEditAdvertisementContent : AdvertisementPageSources -> AF.State -> Html Msgs.Msg
pageEditAdvertisementContent { localeData, hostData } advertisementFormState =
    case ( localeData, hostData ) of
        ( RemoteData.Success locales, RemoteData.Success hosts ) ->
            AF.view
                (AF.Configuration
                    { locales = locales
                    , partnerHosts = hosts
                    , toMsg = Msgs.ChangeAdvertisementForm
                    }
                )
                advertisementFormState

        ( RemoteData.Failure error, _ ) ->
            text <| "Supported locales fetch failed: " ++ (toString error)

        ( _, RemoteData.Failure error ) ->
            text <| "Partner hosts fetch failed: " ++ (toString error)

        _ ->
            text "Loading..."


pageGenerateVoucher : GF.State -> Date -> Html Msgs.Msg
pageGenerateVoucher generateVoucherFormState t =
    div [ class "container" ]
        [ h2 [] [ text "Generate voucher" ]
        , GF.view
            (GF.Configuration
                { toMsg = Msgs.ChangeGenerateVoucherForm
                }
            )
            generateVoucherFormState
        ]


pageRedeemVoucher : RF.State -> Html Msgs.Msg
pageRedeemVoucher redeemVoucherFormState =
    div [ class "container" ]
        [ h2 [] [ text "Redeem voucher" ]
        , RF.view
            (RF.Configuration
                { toMsg = Msgs.ChangeRedeemVoucherForm }
            )
            redeemVoucherFormState
        ]


pageVoucherTable : VT.State -> Html Msgs.Msg
pageVoucherTable voucherTableState =
    div [ class "container" ]
        [ VT.view voucherTableState
        ]


pagePromotions : PM.State -> Html Msgs.Msg
pagePromotions promotionsState =
    div [ class "container" ]
        [ PM.view
            (PM.Configuration
                { toMsg = Msgs.OnPromotionEvent }
            )
            promotionsState
        ]


pageEditPromotion : String -> EF.State -> Html Msgs.Msg
pageEditPromotion id state =
    div [ class "container" ]
        [ h2 []
            [ text "Edit Promotion" ]
        , EF.view
            (EF.Configuration
                { toMsg = Msgs.ChangePromotionForm }
            )
            EF.newState
        ]


pageNotFound : Html msg
pageNotFound =
    div [ class "container" ]
        [ h2 [] [ text "404 Not Found" ] ]


pageLastError : LastError -> Html msg
pageLastError error =
    div [ class "container" ]
        [ h2 [] [ text "Last error" ]
        , case error of
            NoErrorsYet ->
                text "No erros so far. Happy happy joy joy."

            HttpError error ->
                text <| toString error
        ]
