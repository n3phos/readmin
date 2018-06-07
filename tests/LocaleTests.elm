module LocaleTests exposing (all)

import Test exposing (describe, test, Test)
import Expect
import Json.Decode exposing (decodeString)
import Locale exposing (localeRowsDecoder, language, string, LocaleCode(..))


all : Test
all =
    describe "Locale"
        [ describe "localeRowsDecoder"
            [ test "on actual server data" <|
                \() ->
                    Expect.equal
                        (decodeString localeRowsDecoder serverResponseJson)
                        (Ok expectedLocales)
            ]
        , describe "language"
            [ test "on de_CH" <|
                \() ->
                    Expect.equal (language <| LocaleCode "de_CH") "DE"
            ]
        , describe "string"
            [ test "on en_US" <|
                \() -> Expect.equal (string <| LocaleCode "en_US") "en_US"
            ]
        ]


expectedLocales : List LocaleCode
expectedLocales =
    [ LocaleCode "de_CH"
    , LocaleCode "fr_FR"
    , LocaleCode "it_IT"
    ]


serverResponseJson : String
serverResponseJson =
    """
[
    {
        "code": "de_CH",
        "title": "Deutsch",
        "root": "/de/hotels",
        "oldRoot": "/schweiz/hotels"
    },
    {
        "code": "fr_FR",
        "title": "Français",
        "root": "/fr/hotels",
        "oldRoot": "/suisse/hotels"
    },
    {
        "code": "it_IT",
        "title": "Italiano",
        "root": "/it/hotel",
        "oldRoot": "/svizzera/hotel"
    }
]
    """
