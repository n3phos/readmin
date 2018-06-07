module Locale exposing (localeRowsDecoder, language, string, LocaleCode(..))

import Json.Decode as JD


language : LocaleCode -> String
language (LocaleCode code) =
    String.toUpper <| String.left 2 code


string : LocaleCode -> String
string (LocaleCode code) =
    code


localeRowsDecoder : JD.Decoder (List LocaleCode)
localeRowsDecoder =
    JD.list rowDecoder


rowDecoder : JD.Decoder LocaleCode
rowDecoder =
    JD.map LocaleCode (JD.field "code" JD.string)


type LocaleCode
    = LocaleCode String
