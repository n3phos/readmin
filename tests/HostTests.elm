module HostTests exposing (all)

import Test exposing (describe, test, Test)
import Expect
import Json.Decode as JD
import Host exposing (domainByKeyMapDecoder, domain, keyString, Host, HostKey(..))


all : Test
all =
    describe "Host"
        [ describe "domainByKeyMapDecoder"
            [ test "on actual server data" <|
                \() ->
                    Expect.equal
                        (JD.decodeString domainByKeyMapDecoder serverResponseJson)
                        (Ok expectedHosts)
            ]
        , describe "domain"
            [ test "on DEFAULT" <|
                \() -> Expect.equal (domain default) "swisshotels.vps290.xiag.ch"
            ]
        , describe "keyString"
            [ test "on DEFAULT" <|
                \() -> Expect.equal (keyString default) "DEFAULT"
            ]
        ]


expectedHosts : List Host
expectedHosts =
    [ Host (HostKey "CUMULUS150") "cumulus150.vps290.xiag.ch"
    , Host (HostKey "DELUXE") "deluxe.vps290.xiag.ch"
    , default
    ]


default : Host
default =
    Host (HostKey "DEFAULT") "swisshotels.vps290.xiag.ch"


serverResponseJson : String
serverResponseJson =
    """
{
  "CUMULUS150": "cumulus150.vps290.xiag.ch",
  "DEFAULT": "swisshotels.vps290.xiag.ch",
  "DELUXE": "deluxe.vps290.xiag.ch"
}
    """
