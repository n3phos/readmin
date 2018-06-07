module Advertisements exposing (advertisementRowsDecoder, advertisementRowDecoder, view)

import Json.Decode as JD
import Html exposing (..)
import Html.Attributes exposing (class, type_, colspan, style, href)
import Html.Events exposing (onClick)
import Advertisements.Model exposing (..)
import DateDecoder exposing (dateDecoder)
import FormatDate exposing (formatDate)
import Locale exposing (language, LocaleCode(..))
import Region exposing (Region, RegionId(..))
import Host exposing (HostKey(..))
import Msgs


-- JSON


advertisementRowsDecoder : JD.Decoder (List Advertisement)
advertisementRowsDecoder =
    JD.field
        "rows"
        (JD.list (JD.field "value" advertisementRowDecoder))


advertisementRowDecoder : JD.Decoder Advertisement
advertisementRowDecoder =
    JD.map2
        Advertisement
        adIdentifierDecoder
        adDataDecoder


adIdentifierDecoder : JD.Decoder AdvertisementId
adIdentifierDecoder =
    JD.map2
        AdvertisementId
        (JD.map AdvertisementIdKey (JD.field "_id" JD.string))
        (JD.map AdvertisementIdRevision (JD.field "_rev" JD.string))


adDataDecoder : JD.Decoder AdvertisementData
adDataDecoder =
    JD.map5
        AdvertisementData
        (JD.field "title" JD.string)
        (JD.field "content" JD.string)
        datesIntervalDecoder
        hotelConstraintsDecoder
        siteConstraintsDecoder


datesIntervalDecoder : JD.Decoder DatesInterval
datesIntervalDecoder =
    JD.map2 DatesInterval (JD.field "dateFrom" dateDecoder) (JD.field "dateTo" dateDecoder)


hotelConstraintsDecoder : JD.Decoder AdvertisementHotelConstraints
hotelConstraintsDecoder =
    JD.map2 AdvertisementHotelConstraints
        hotelRegionDecoder
        (JD.field "rateCode" rateCodesDecoder)


hotelRegionDecoder : JD.Decoder AdvertisementRegion
hotelRegionDecoder =
    JD.field "region"
        (JD.oneOf
            [ JD.map (AdvertisementRegionOnly << RegionId) JD.string
            , JD.null AdvertisementRegionAny
            ]
        )


rateCodesDecoder : JD.Decoder AdvertisementRateCodes
rateCodesDecoder =
    JD.oneOf [ JD.string, JD.null "" ]
        |> JD.map
            (\x ->
                if x == "" then
                    AdvertisementRateCodesAll
                else
                    let
                        ( h, t ) =
                            simplisticCsv x
                    in
                        AdvertisementRateCodesOnly (RateCode h) (List.map RateCode t)
            )


siteConstraintsDecoder : JD.Decoder AdvertisementSiteConstraints
siteConstraintsDecoder =
    JD.map4 AdvertisementSiteConstraints
        (JD.map HostKey <| JD.field "engine" JD.string)
        countriesDecoder
        (JD.map LocaleCode <| JD.field "locale" JD.string)
        mediumDecoder


countriesDecoder : JD.Decoder AdvertisementCountries
countriesDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\x ->
                case x of
                    "only" ->
                        countryValueOnlyDecoder

                    "except" ->
                        countryValueExceptDecoder

                    _ ->
                        JD.succeed AdvertisementCountriesAll
            )


countryValueOnlyDecoder : JD.Decoder AdvertisementCountries
countryValueOnlyDecoder =
    makeCountryValueDecoder AdvertisementCountriesOnly


countryValueExceptDecoder : JD.Decoder AdvertisementCountries
countryValueExceptDecoder =
    makeCountryValueDecoder AdvertisementCountriesExceptFor


makeCountryValueDecoder :
    (CountryCode -> List CountryCode -> AdvertisementCountries)
    -> JD.Decoder AdvertisementCountries
makeCountryValueDecoder constructor =
    JD.field "country" JD.string
        |> JD.map
            (\x ->
                let
                    ( h, t ) =
                        simplisticCsv x
                in
                    constructor (CountryCode h) (List.map CountryCode t)
            )


mediumDecoder : JD.Decoder AdvertisementMedium
mediumDecoder =
    JD.map
        (\x ->
            if x == "confirmationPage" then
                ConfirmationPage
            else
                ConfirmationEmail
        )
        (JD.field "place" JD.string)


simplisticCsv : String -> ( String, List String )
simplisticCsv x =
    case String.split "," x of
        h :: t ->
            ( h, t )

        [] ->
            ( x, [] )



-- VIEW


view : List Advertisement -> Html Msgs.Msg
view ads =
    div []
        [ topButtonContainer
        , table [ class "table" ] [ tableHeader, tableBody ads ]
        ]


topButtonContainer : Html msg
topButtonContainer =
    div [ class "row justify-content-end", style [ ( "margin-bottom", "1em" ) ] ]
        [ a
            [ href "#new-advertisement", class "btn btn-success btn-lg text-right" ]
            [ text "New advertisement" ]
        ]


tableHeader : Html msg
tableHeader =
    thead []
        [ tr []
            [ th [] [ text "Duration" ]
            , th [] [ text "Title" ]
            , th [] [ text "Language" ]
            , th [] [ text "Carrier" ]
            , th [] [ text "Partner host" ]
            , th [] [ text "Countries" ]
            , th [] [ text "Hotel region" ]
            , th [] [ text "Hotel rates" ]
            , th [ colspan 2 ] []
            ]
        ]


tableBody : List Advertisement -> Html Msgs.Msg
tableBody ads =
    tbody [] (List.map tableRow ads)


tableRow : Advertisement -> Html Msgs.Msg
tableRow { id, data } =
    let
        { key } =
            id
    in
        tr []
            [ td [] [ text <| duration data.datesInterval ]
            , td [] [ text data.title ]
            , td [] [ text <| language data.siteConstraints.locale ]
            , td [] [ text <| carrier data.siteConstraints.place ]
            , td [] [ text <| partnerHost data.siteConstraints.engine ]
            , td [] [ text <| countries data.siteConstraints.countries ]
            , td [] [ text <| region data.hotelConstraints.region ]
            , td [] [ text <| rateCodes data.hotelConstraints.rateCodes ]
            , td [] [ a [ href (adRoute key), class "btn btn-primary" ] [ text "Edit" ] ]
            , td []
                [ button
                    [ type_ "button"
                    , class "btn btn-danger"
                    , onClick <| Msgs.DeleteAdvertisementAsk id data.title
                    ]
                    [ text "Delete" ]
                ]
            ]


duration : DatesInterval -> String
duration duration =
    formatDate duration.firstDate ++ " — " ++ formatDate duration.lastDate


carrier : AdvertisementMedium -> String
carrier place =
    case place of
        ConfirmationEmail ->
            "confirmation e-mail message"

        ConfirmationPage ->
            "confirmation page"


partnerHost : HostKey -> String
partnerHost (HostKey key) =
    key


countries : AdvertisementCountries -> String
countries x =
    case x of
        AdvertisementCountriesAll ->
            "all"

        AdvertisementCountriesOnly h t ->
            "only " ++ csv (\(CountryCode x) -> x) h t

        AdvertisementCountriesExceptFor h t ->
            "all except for " ++ csv (\(CountryCode x) -> x) h t


region : AdvertisementRegion -> String
region ar =
    case ar of
        AdvertisementRegionAny ->
            "Any"

        AdvertisementRegionOnly (RegionId x) ->
            case Region.name (RegionId x) of
                Just s ->
                    s ++ " - " ++ x

                Nothing ->
                    x


rateCodes : AdvertisementRateCodes -> String
rateCodes x =
    case x of
        AdvertisementRateCodesAll ->
            "all"

        AdvertisementRateCodesOnly h t ->
            "only " ++ csv (\(RateCode x) -> x) h t


adRoute : AdvertisementIdKey -> String
adRoute (AdvertisementIdKey id) =
    "#advertisements/" ++ id


csv : (a -> String) -> a -> List a -> String
csv extract head tail =
    let
        headString =
            extract head

        tailStrings =
            List.map extract tail

        allStrings =
            headString :: tailStrings
    in
        String.join ", " allStrings
