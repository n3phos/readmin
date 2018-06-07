module AdvertisementsTests exposing (all)

import Test exposing (describe, test, Test)
import Expect
import Date exposing (Date)
import Date.Extra.Create exposing (dateFromFields)
import Json.Decode exposing (decodeString)
import Advertisements exposing (advertisementRowsDecoder)
import Advertisements.Model exposing (..)
import Region exposing (RegionId(..))
import Host exposing (HostKey(..))
import Locale exposing (LocaleCode(..))


all : Test
all =
    describe "Advertisements.advertisementRowsDecoder"
        [ test "on actual CouchDB data" <|
            \() ->
                Expect.equal
                    (decodeString advertisementRowsDecoder serverResponseJson)
                    (Ok expectedAdvertisements)
        ]


expectedAdvertisements : List Advertisement
expectedAdvertisements =
    [ Advertisement
        (AdvertisementId
            (AdvertisementIdKey "09772bfae6f68165e562475ebfbecc58")
            (AdvertisementIdRevision "7-42c5362eca449ad935c1afeeb8650575")
        )
        (AdvertisementData
            "Lago Maggiore Flyer"
            "French content"
            theDatesInterval
            (AdvertisementHotelConstraints
                (AdvertisementRegionOnly (RegionId "1279"))
                AdvertisementRateCodesAll
            )
            (AdvertisementSiteConstraints
                (HostKey "SBB")
                AdvertisementCountriesAll
                (LocaleCode "fr_FR")
                ConfirmationEmail
            )
        )
    , Advertisement
        (AdvertisementId
            (AdvertisementIdKey "3360ebe37988f4a36f6cc580c5863c0e")
            (AdvertisementIdRevision "3-bee64f73bcd6007adc0639aebd34dd58")
        )
        (AdvertisementData
            "Lago Maggiore Flyer"
            "Italian content"
            theDatesInterval
            (AdvertisementHotelConstraints
                (AdvertisementRegionOnly (RegionId "1279"))
                (AdvertisementRateCodesOnly (RateCode "RAC") [ RateCode "NAP" ])
            )
            (AdvertisementSiteConstraints
                (HostKey "SBB")
                (AdvertisementCountriesExceptFor (CountryCode "RU") [])
                (LocaleCode "it_IT")
                ConfirmationPage
            )
        )
    ]


theDatesInterval : DatesInterval
theDatesInterval =
    DatesInterval
        (dateFromFields 2017 Date.Mar 20 1 0 0 0)
        (dateFromFields 2017 Date.Aug 31 2 0 0 0)


serverResponseJson : String
serverResponseJson =
    """
{
    "total_rows": 4,
    "offset": 0,
    "rows": [
        {
            "id": "09772bfae6f68165e562475ebfbecc58",
            "key": null,
            "value": {
                "_id": "09772bfae6f68165e562475ebfbecc58",
                "_rev": "7-42c5362eca449ad935c1afeeb8650575",
                "title": "Lago Maggiore Flyer",
                "content": "French content",
                "dateFrom": "2017-03-20",
                "dateTo": "2017-08-31",
                "locale": "fr_FR",
                "country": null,
                "region": "1279",
                "engine": "SBB",
                "rateCode": "",
                "place": "email",
                "type": "all"
            }
        },
        {
            "id": "3360ebe37988f4a36f6cc580c5863c0e",
            "key": null,
            "value": {
                "_id": "3360ebe37988f4a36f6cc580c5863c0e",
                "_rev": "3-bee64f73bcd6007adc0639aebd34dd58",
                "title": "Lago Maggiore Flyer",
                "content": "Italian content",
                "dateFrom": "2017-03-20",
                "dateTo": "2017-08-31",
                "locale": "it_IT",
                "country": "RU",
                "region": "1279",
                "engine": "SBB",
                "rateCode": "RAC,NAP",
                "place": "confirmationPage",
                "type": "except"
            }
        }
    ]
}
"""
