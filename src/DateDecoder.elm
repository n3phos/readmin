module DateDecoder exposing (dateDecoder, dateTimeDecoder)

import Date exposing (Date)
import Json.Decode as JD
import FormatDate exposing (..)


dateDecoder : JD.Decoder Date
dateDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case Date.fromString str of
                    Err err ->
                        JD.fail
                            ("Can't decode Date from " ++ str ++ ": " ++ err)

                    Ok date ->
                        JD.succeed date
            )


dateTimeDecoder : JD.Decoder String
dateTimeDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case Date.fromString str of
                    Err err ->
                        JD.fail
                            ("Can't decode Date from " ++ str ++ ": " ++ err)

                    Ok date ->
                        JD.succeed (formatVoucherDate date)
            )
