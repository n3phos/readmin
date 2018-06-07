module BookingStatsTests exposing (all)

import Test exposing (describe, test, Test)
import Expect
import Date exposing (Date)
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Duration as Duration
import BookingStats exposing (range, BookingStatsRecord)


all : Test
all =
    describe "BookingStats.range"
        [ test "on the zero range size" <|
            \() -> Expect.equal (range theDay [] 0) []
        , test "on none of the non-zero records present, length 1" <|
            \() -> Expect.equal (range theDay [] 1) [ zeroRecord theDay ]
        , test "on none of the non-zero records present, length 2" <|
            \() ->
                Expect.equal
                    (range theDay [] 2)
                    [ zeroRecord (delta -1), zeroRecord theDay ]
        , test "works in a normal case" <|
            \() ->
                Expect.equal
                    (range theDay [ record1, record2 ] 4)
                    [ zeroRecord (delta -3), record1, record2, zeroRecord theDay ]
        ]


zeroRecord : Date -> BookingStatsRecord
zeroRecord date =
    BookingStatsRecord date 0 0 0.0


delta : Int -> Date
delta days =
    Duration.add Duration.Day days theDay


theDay : Date
theDay =
    dateFromFields 2000 Date.Mar 13 13 13 13 0


record1 : BookingStatsRecord
record1 =
    BookingStatsRecord (dateFromFields 2000 Date.Mar 11 0 0 0 0) 1 1 100.0


record2 : BookingStatsRecord
record2 =
    BookingStatsRecord (dateFromFields 2000 Date.Mar 12 0 0 0 0) 4 2 877.0
