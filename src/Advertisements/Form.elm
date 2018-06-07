module Advertisements.Form
    exposing
        ( newState
        , view
        , Configuration(..)
        , State
        , OnInput
        , update
        , stateFromModel
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, onClick, onSubmit)
import Json.Decode as JD
import Json.Encode as JE
import Date exposing (Date)
import String.Extra exposing (humanize)
import Advertisements.Model exposing (..)
import Locale exposing (LocaleCode(..))
import Host exposing (Host, HostKey(..))
import Region exposing (Region, RegionId(..))
import FormatDate


type Configuration msg
    = Configuration
        { locales : List LocaleCode
        , partnerHosts : List Host
        , toMsg : OnInput -> msg
        }


type UserInteraction
    = UserEditing
    | UserSaving


type alias State =
    { values : Values, decodedValues : DecodedValues, errors : Errors, userInteraction : UserInteraction }


type alias Values =
    { firstDate : String
    , lastDate : String
    , title : String
    , locale : String
    , carrier : Carrier
    , partnerHost : String
    , hotelRegion : String
    , content : String
    }


type alias DecodedValues =
    { firstDate : Date
    , lastDate : Date
    }


type alias Errors =
    { firstDate : DateError, lastDate : DateError, interval : DateIntervalError }


type DateError
    = NoDateError
    | InvalidDate


type DateIntervalError
    = NoDateIntervalError
    | InvalidDateInterval


type Carrier
    = NotSelected
    | Selected AdvertisementMedium


type Msg
    = OnInput
    | Submit AdvertisementData


type OnInput
    = OnInputFirstDate String
    | OnInputLastDate String
    | OnInputTitle String
    | OnInputLocale String
    | OnInputCarrier Carrier
    | OnInputPartnerHost String
    | OnInputHotelRegion String
    | OnInputContent String
    | Validate


newState : State
newState =
    State
        (Values "" "" "" "" NotSelected "" "" "")
        (DecodedValues (Date.fromTime 0) (Date.fromTime 0))
        (Errors NoDateError NoDateError NoDateIntervalError)
        UserEditing


stateFromModel : AdvertisementData -> State
stateFromModel { title, content, datesInterval, hotelConstraints, siteConstraints } =
    State
        (Values
            (FormatDate.toString datesInterval.firstDate)
            (FormatDate.toString datesInterval.lastDate)
            title
            (Locale.string siteConstraints.locale)
            (Selected siteConstraints.place)
            (Host.keyString <| Host siteConstraints.engine "")
            (advertisementRegionString hotelConstraints.region)
            content
        )
        (DecodedValues datesInterval.firstDate datesInterval.lastDate)
        (Errors NoDateError NoDateError NoDateIntervalError)
        UserEditing


advertisementRegionString : AdvertisementRegion -> String
advertisementRegionString region =
    case region of
        AdvertisementRegionAny ->
            "AdvertisementRegionAny"

        AdvertisementRegionOnly regionId ->
            Region.idString <| Region regionId ""



-- UPDATE


update : (JE.Value -> Cmd msg) -> OnInput -> State -> ( State, Cmd msg )
update submitCmd msg state =
    let
        { values, errors } =
            state
    in
        case msg of
            OnInputFirstDate x ->
                updateFirstDate x state

            OnInputLastDate x ->
                updateLastDate x state

            OnInputTitle x ->
                ( updateValues { values | title = x } state, Cmd.none )

            OnInputLocale x ->
                ( updateValues { values | locale = x } state, Cmd.none )

            OnInputCarrier x ->
                ( updateValues { values | carrier = x } state, Cmd.none )

            OnInputPartnerHost x ->
                ( updateValues { values | partnerHost = x } state, Cmd.none )

            OnInputHotelRegion x ->
                ( updateValues { values | hotelRegion = x } state, Cmd.none )

            OnInputContent x ->
                ( updateValues { values | content = x } state, Cmd.none )

            Validate ->
                let
                    newState =
                        validate state
                in
                    ( newState, commandAfterValidation submitCmd newState )


updateFirstDate : String -> State -> ( State, Cmd msg )
updateFirstDate x state =
    let
        { values, errors } =
            state

        updateValuesAndErrors =
            updateValues { values | firstDate = x }
                >> updateErrors
                    { errors
                        | firstDate = NoDateError
                        , interval = NoDateIntervalError
                    }
    in
        ( updateValuesAndErrors state, Cmd.none )


updateLastDate : String -> State -> ( State, Cmd msg )
updateLastDate x state =
    let
        { values, errors } =
            state

        updateValuesAndErrors =
            updateValues { values | lastDate = x }
                >> updateErrors
                    { errors
                        | lastDate = NoDateError
                        , interval = NoDateIntervalError
                    }
    in
        ( updateValuesAndErrors state, Cmd.none )


updateValues : Values -> State -> State
updateValues values state =
    { state | values = values }


validate : State -> State
validate state =
    let
        validated =
            (validateInterval << validateLastDate << validateFirstDate) state
    in
        if validated.errors == Errors NoDateError NoDateError NoDateIntervalError then
            { validated | userInteraction = UserSaving }
        else
            validated


validateFirstDate : State -> State
validateFirstDate state =
    let
        { values, errors } =
            state
    in
        case Date.fromString values.firstDate of
            Ok _ ->
                state

            Err _ ->
                updateErrors { errors | firstDate = InvalidDate } state


validateLastDate : State -> State
validateLastDate state =
    let
        { values, errors } =
            state
    in
        case Date.fromString values.lastDate of
            Ok _ ->
                state

            Err _ ->
                updateErrors { errors | lastDate = InvalidDate } state


validateInterval : State -> State
validateInterval state =
    let
        { values, errors } =
            state
    in
        case ( Date.fromString values.firstDate, Date.fromString values.lastDate ) of
            ( Ok a, Ok b ) ->
                if Date.toTime a <= Date.toTime b then
                    { state | decodedValues = DecodedValues a b }
                else
                    updateErrors { errors | interval = InvalidDateInterval } state

            _ ->
                state


updateErrors : Errors -> State -> State
updateErrors errors state =
    { state | errors = errors }


commandAfterValidation : (JE.Value -> Cmd msg) -> State -> Cmd msg
commandAfterValidation submitCmd { values, decodedValues, errors } =
    case ( errors.firstDate, errors.lastDate, errors.interval ) of
        ( NoDateError, NoDateError, NoDateIntervalError ) ->
            toAdvertisementData values decodedValues
                |> toJsonValue
                |> submitCmd

        _ ->
            Cmd.none


toJsonValue : AdvertisementData -> JE.Value
toJsonValue advertisementData =
    let
        (HostKey engine) =
            advertisementData.siteConstraints.engine

        (LocaleCode locale) =
            advertisementData.siteConstraints.locale

        place =
            if advertisementData.siteConstraints.place == ConfirmationPage then
                "confirmationPage"
            else
                "email"

        regionValue =
            case advertisementData.hotelConstraints.region of
                AdvertisementRegionAny ->
                    JE.null

                AdvertisementRegionOnly (RegionId id) ->
                    JE.string id
    in
        JE.object
            [ ( "content", JE.string advertisementData.content )
            , ( "country", JE.null )
            , ( "dateFrom"
              , JE.string <|
                    FormatDate.toString advertisementData.datesInterval.firstDate
              )
            , ( "dateTo"
              , JE.string <|
                    FormatDate.toString advertisementData.datesInterval.lastDate
              )
            , ( "engine", JE.string engine )
            , ( "locale", JE.string locale )
            , ( "place", JE.string place )
            , ( "rateCode", JE.null )
            , ( "region", regionValue )
            , ( "title", JE.string advertisementData.title )
            , ( "type", JE.string "all" )
            ]


toAdvertisementData : Values -> DecodedValues -> AdvertisementData
toAdvertisementData values decodedValues =
    AdvertisementData
        values.title
        values.content
        (DatesInterval decodedValues.firstDate decodedValues.lastDate)
        (AdvertisementHotelConstraints
            (toAdvertisementRegion values.hotelRegion)
            AdvertisementRateCodesAll
        )
        (AdvertisementSiteConstraints
            (HostKey values.partnerHost)
            AdvertisementCountriesAll
            (LocaleCode values.locale)
            (if values.carrier == Selected ConfirmationPage then
                ConfirmationPage
             else
                ConfirmationEmail
            )
        )


toAdvertisementRegion : String -> AdvertisementRegion
toAdvertisementRegion s =
    case s of
        "AdvertisementRegionAny" ->
            AdvertisementRegionAny

        _ ->
            AdvertisementRegionOnly <| RegionId s



-- VIEW


view : Configuration msg -> State -> Html msg
view (Configuration { locales, partnerHosts, toMsg }) state =
    Html.map toMsg <|
        Html.form
            [ style [ ( "margin-bottom", "5rem" ) ], onSubmit Validate ]
            ([ firstDateInput state.errors state.values.firstDate
             , lastDateInput state.errors state.values.lastDate
             , titleInput state.values.title
             , languageSelect locales state.values.locale
             , carrierInput state.values.carrier
             , partnerHostSelect partnerHosts state.values.partnerHost
             , hotelRegionSelect state.values.hotelRegion
             , contentInput state.values.content
             ]
                ++ saveControls state.userInteraction
            )


saveControls : UserInteraction -> List (Html msg)
saveControls interaction =
    [ button [ type_ "submit", class "btn btn-primary", disabled <| interaction == UserSaving ] [ text "Save" ]
    , savingBadge interaction
    ]


savingBadge : UserInteraction -> Html msg
savingBadge interaction =
    if interaction == UserSaving then
        span
            [ class "badge badge-info text-right", style [ ( "float", "right" ) ] ]
            [ text "Saving…" ]
    else
        Html.text ""


firstDateInput : Errors -> String -> Html OnInput
firstDateInput { firstDate, interval } currentValue =
    dateInput
        OnInputFirstDate
        "firstDate"
        (case ( firstDate, interval ) of
            ( NoDateError, NoDateIntervalError ) ->
                []

            ( InvalidDate, _ ) ->
                [ "Impossible in Gregorian calendar" ]

            ( _, InvalidDateInterval ) ->
                [ "The first date can't be after the last date" ]
        )
        currentValue


lastDateInput : Errors -> String -> Html OnInput
lastDateInput { lastDate, interval } currentValue =
    dateInput
        OnInputLastDate
        "lastDate"
        (case ( lastDate, interval ) of
            ( NoDateError, NoDateIntervalError ) ->
                []

            ( InvalidDate, _ ) ->
                [ "Impossible in Gregorian calendar" ]

            ( _, InvalidDateInterval ) ->
                [ "The last date can't be before the first date" ]
        )
        currentValue


dateInput : (String -> OnInput) -> String -> List String -> String -> Html OnInput
dateInput constructOwnMsg inputName errorMessages currentValue =
    div
        [ classList
            [ ( "form-group", True )
            , ( "has-danger", List.length errorMessages > 0 )
            ]
        ]
        ([ label
            [ for <| inputId inputName, class "form-control-label" ]
            [ text <| humanize inputName ]
         , input
            [ id <| inputId inputName
            , class "form-control"
            , required True
            , type_ "date"
            , placeholder "YYYY-MM-DD"
            , pattern "^[0-9]{4}-[0-1][0-9]-[0-3][0-9]$"
            , title "YYYY-MM-DD year, then month, then day"
            , onInput constructOwnMsg
            , defaultValue currentValue
            ]
            []
         ]
            ++ (List.map
                    (\x -> div [ class "form-control-feedback" ] [ text x ])
                    errorMessages
               )
        )


titleInput : String -> Html OnInput
titleInput currentValue =
    wrapControl "title" input [ type_ "text", onInput OnInputTitle, defaultValue currentValue ] []


languageSelect : List LocaleCode -> String -> Html OnInput
languageSelect locales currentValue =
    wrapControl
        "language"
        select
        [ on "change" <| selectStringMessageDecoder OnInputLocale ]
        (languageOptions locales currentValue)


languageOptions : List LocaleCode -> String -> List (Html msg)
languageOptions locales currentValue =
    option
        [ value ""
        , hidden True
        , disabled True
        , selected (currentValue == "")
        ]
        [ text "(please select)" ]
        :: List.map
            (\l ->
                option
                    [ value <| Locale.string l
                    , selected (currentValue == Locale.string l)
                    ]
                    [ text <| Locale.language l ]
            )
            locales


contentInput : String -> Html OnInput
contentInput currentValue =
    wrapControl "content" textarea [ rows 8, onInput OnInputContent ] [ text currentValue ]


carrierInput : Carrier -> Html OnInput
carrierInput currentValue =
    fieldset [ class "form-group" ]
        [ legend [] [ text "Carrier" ]
        , carrierOption
            (OnInputCarrier <| Selected ConfirmationEmail)
            "confirmationEmailMessage"
            (currentValue == Selected ConfirmationEmail)
        , carrierOption (OnInputCarrier <| Selected ConfirmationPage)
            "confirmationPage"
            (currentValue == Selected ConfirmationPage)
        ]


carrierOption : OnInput -> String -> Bool -> Html OnInput
carrierOption ownMessage optionValue isChecked =
    div [ class "form-check" ]
        [ label [ class "form-check-label" ]
            [ input
                ([ type_ "radio"
                 , name "carrier"
                 , defaultValue optionValue
                 , class "form-check-input"
                 , required True
                 , onClick ownMessage
                 ]
                    ++ if isChecked then
                        [ checked True ]
                       else
                        []
                )
                []
            , text <| " " ++ humanize optionValue
            ]
        ]


partnerHostSelect : List Host -> String -> Html OnInput
partnerHostSelect hosts currentValue =
    wrapControl
        "partnerHost"
        select
        [ on "change" <| selectStringMessageDecoder OnInputPartnerHost ]
        (partnerHostOptions hosts currentValue)


partnerHostOptions : List Host -> String -> List (Html msg)
partnerHostOptions hosts currentValue =
    option
        [ value ""
        , hidden True
        , disabled True
        , selected (currentValue == "")
        ]
        [ text "(please select)" ]
        :: List.map
            (\h ->
                option
                    [ value <| Host.keyString h
                    , selected (Host.keyString h == currentValue)
                    ]
                    [ text <| partnerHostOptionText h ]
            )
            hosts


partnerHostOptionText : Host -> String
partnerHostOptionText h =
    h.domain ++ " - " ++ Host.keyString h


hotelRegionSelect : String -> Html OnInput
hotelRegionSelect currentValue =
    wrapControl
        "hotelRegion"
        select
        [ on "change" <| selectStringMessageDecoder OnInputHotelRegion ]
        (hotelRegionOptions currentValue)


hotelRegionOptions : String -> List (Html msg)
hotelRegionOptions currentValue =
    [ option
        [ value ""
        , hidden True
        , disabled True
        , selected (currentValue == "")
        ]
        [ text "(please select)" ]
    , option
        [ value <| toString AdvertisementRegionAny
        , selected (toString AdvertisementRegionAny == currentValue)
        ]
        [ text "ANY" ]
    ]
        ++ List.map
            (\r ->
                option
                    [ value <| Region.idString r
                    , selected (Region.idString r == currentValue)
                    ]
                    [ text <| hotelRegionOptionText r ]
            )
            Region.all


hotelRegionOptionText : Region -> String
hotelRegionOptionText r =
    r.name ++ " - " ++ Region.idString r


wrapControl :
    String
    -> (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
wrapControl controlName controlFactory extraAttributes children =
    div [ class "form-group" ]
        [ label [ for <| inputId controlName ] [ text <| humanize controlName ]
        , controlFactory
            ([ id <| inputId controlName, class "form-control", required True ] ++ extraAttributes)
            children
        ]


inputId : String -> String
inputId inputName =
    "input-" ++ inputName


selectStringMessageDecoder : (String -> msg) -> JD.Decoder msg
selectStringMessageDecoder toMsg =
    let
        targetValue : JD.Decoder String
        targetValue =
            JD.at [ "target", "value" ] JD.string
    in
        JD.map toMsg targetValue
