module Advertisements.Model exposing (..)

import Date exposing (Date)
import Locale exposing (LocaleCode)
import Host exposing (HostKey)
import Region exposing (RegionId)


type alias Advertisement =
    { id : AdvertisementId
    , data : AdvertisementData
    }


type alias AdvertisementData =
    { title : String
    , content : String
    , datesInterval : DatesInterval
    , hotelConstraints : AdvertisementHotelConstraints
    , siteConstraints : AdvertisementSiteConstraints
    }


type alias AdvertisementId =
    { key : AdvertisementIdKey, revision : AdvertisementIdRevision }


type AdvertisementIdKey
    = AdvertisementIdKey String


type AdvertisementIdRevision
    = AdvertisementIdRevision String


type alias DatesInterval =
    { firstDate : Date
    , lastDate : Date
    }


type alias AdvertisementHotelConstraints =
    { region : AdvertisementRegion
    , rateCodes : AdvertisementRateCodes
    }


type alias AdvertisementSiteConstraints =
    { engine : HostKey
    , countries : AdvertisementCountries
    , locale : LocaleCode
    , place : AdvertisementMedium
    }


type AdvertisementRegion
    = AdvertisementRegionAny
    | AdvertisementRegionOnly RegionId


type AdvertisementRateCodes
    = AdvertisementRateCodesAll
    | AdvertisementRateCodesOnly RateCode (List RateCode)


type AdvertisementCountries
    = AdvertisementCountriesAll
    | AdvertisementCountriesOnly CountryCode (List CountryCode)
    | AdvertisementCountriesExceptFor CountryCode (List CountryCode)


type CountryCode
    = CountryCode String


type RateCode
    = RateCode String


type AdvertisementMedium
    = ConfirmationPage
    | ConfirmationEmail
