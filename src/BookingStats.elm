module BookingStats exposing (recordRowsDecoder, view, range, limitDays, BookingStatsRecord)

import Date exposing (Date)
import Json.Decode as JD
import Html exposing (..)
import Html.Attributes exposing (class)
import FormatNumber as NF
import FormatNumber.Locales exposing (Locale)
import Plot
import Date.Extra.Duration as Duration
import BookingStats.MergeByDate exposing (mergeByDate)
import DateDecoder exposing (dateDecoder)
import FormatDate exposing (formatDate)


type alias BookingStatsRecord =
    { date : Date
    , rooms : Int
    , orders : Int
    , revenueChf : Float
    }



-- JSON


recordDecoder : JD.Decoder BookingStatsRecord
recordDecoder =
    JD.map4
        BookingStatsRecord
        (JD.field "key" dateDecoder)
        (JD.at [ "value", "rooms" ] JD.int)
        (JD.at [ "value", "orders" ] JD.int)
        (JD.at [ "value", "revenue" ] JD.float)


recordRowsDecoder : JD.Decoder (List BookingStatsRecord)
recordRowsDecoder =
    JD.field "rows" (JD.list recordDecoder)



-- RANGE


{-| Creates a list of rangeLengthDays BookingStatsRecord-s with consecutive days as the records'
`date` value. The last day will be the upToDate. presentRecords are the non-zero aggregated per-day
sales figures fetched from the database. Basically, this function fills in the date gaps in sales
with "zero records", presenting the sales figures for the rangeLengthDays-long continuous interval
of dates, ending on the upToDate.
-}
range : Date -> List BookingStatsRecord -> Int -> List BookingStatsRecord
range upToDate presentRecords rangeLengthDays =
    List.range ((negate rangeLengthDays) + 1) 0
        |> List.map (\x -> delta upToDate x)
        |> zerosRange
        |> mergeByDate presentRecords


zerosRange : List Date -> List BookingStatsRecord
zerosRange dates =
    List.map (\date -> BookingStatsRecord date 0 0 0.0) dates


delta : Date -> Int -> Date
delta base daysToAdd =
    Duration.add Duration.Day daysToAdd base



-- VIEW


view : List BookingStatsRecord -> Html msg
view records =
    figure [ class "figure container" ]
        [ figcaption
            [ class "text-center figure-caption font-weight-bold" ]
            [ text ("Revenue for the last " ++ toString limitDays ++ " days (CHF)") ]
        , chart records
        , table [ class "table table-sm table-bordered" ] [ tableHeader, tableBody records ]
        ]


limitDays : Int
limitDays =
    100


chart : List BookingStatsRecord -> Html msg
chart records =
    Plot.viewBarsCustom
        chartCustomizations
        (Plot.histogram (List.map (\data -> Plot.group "" data)))
        (chartSeries records)


chartCustomizations : Plot.PlotCustomizations msg
chartCustomizations =
    let
        base =
            Plot.defaultBarsPlotCustomizations
    in
        { base
            | margin = { left = 64, top = 16, right = 64, bottom = 16 }
            , width = 800
            , height = 450
        }


chartSeries : List BookingStatsRecord -> List (List Float)
chartSeries records =
    List.map (\r -> [ r.revenueChf ]) records


tableHeader : Html msg
tableHeader =
    thead []
        [ tr []
            [ th [] [ text "Date" ]
            , th [] [ text "Orders" ]
            , th [] [ text "Rooms" ]
            , th [] [ text "Revenue CHF" ]
            ]
        ]


tableBody : List BookingStatsRecord -> Html msg
tableBody records =
    tbody [] (List.map tableRow <| List.reverse records)


tableRow : BookingStatsRecord -> Html msg
tableRow record =
    tr []
        [ td [] [ text (formatDate record.date) ]
        , td [] [ text (toString record.orders) ]
        , td [] [ text (toString record.rooms) ]
        , td [ class "text-right" ] [ text (NF.format amountLocale record.revenueChf) ]
        ]


amountLocale : Locale
amountLocale =
    { decimals = 2
    , thousandSeparator = "'"
    , decimalSeparator = "."
    }
