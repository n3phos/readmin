module BookingStats.MergeByDate exposing (mergeByDate)

import Date exposing (Date)
import Dict exposing (Dict)
import Date.Extra.Core exposing (monthToInt)


type alias Dated a =
    { a | date : Date }


type alias YearNum =
    Int


type alias MonthNum =
    Int


type alias DayNum =
    Int


type alias Key =
    ( YearNum, MonthNum, DayNum )


mergeByDate : List (Dated a) -> List (Dated a) -> List (Dated a)
mergeByDate source target =
    let
        index =
            makeIndex source
    in
        List.map
            (\dated ->
                case Dict.get (key dated) index of
                    Just s ->
                        s

                    Nothing ->
                        dated
            )
            target


makeIndex : List (Dated a) -> Dict Key (Dated a)
makeIndex ds =
    List.foldr (\d agg -> (Dict.insert (key d) d agg)) Dict.empty ds


key : Dated a -> Key
key dated =
    let
        date =
            dated.date
    in
        ( Date.year date, Date.month date |> monthToInt, Date.day date )
