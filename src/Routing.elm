module Routing exposing (parseLocation, Route(..))

import Navigation exposing (Location)
import Advertisements.Model exposing (AdvertisementIdKey(..))
import UrlParser exposing (..)
import Promotions.Model exposing (..)


type Route
    = BookingStatsRoute
    | AdvertisementsRoute
    | AdvertisementRoute AdvertisementIdKey
    | NewAdvertisementRoute
    | LastErrorRoute
    | NotFoundRoute
    | GenerateVoucherRoute
    | RedeemVoucherRoute
    | VoucherTableRoute
    | PromotionsRoute
    | PromotionRoute PromotionId


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map BookingStatsRoute top
        , map BookingStatsRoute (s "dashboard")
        , map (AdvertisementRoute << AdvertisementIdKey) (s "advertisements" </> string)
        , map AdvertisementsRoute (s "advertisements")
        , map NewAdvertisementRoute (s "new-advertisement")
        , map LastErrorRoute (s "last-error")
        , map GenerateVoucherRoute (s "vouchers" </> s "generate")
        , map RedeemVoucherRoute (s "vouchers" </> s "redeem")
        , map VoucherTableRoute (s "vouchers" </> s "table")
        , map PromotionsRoute (s "promotions")
        , map PromotionRoute (s "promotions" </> string)

        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
