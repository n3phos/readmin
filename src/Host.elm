module Host exposing (domainByKeyMapDecoder, domain, keyString, Host, HostKey(..))

import Json.Decode as JD


type alias Host =
    { key : HostKey, domain : String }


type HostKey
    = HostKey String


domainByKeyMapDecoder : JD.Decoder (List Host)
domainByKeyMapDecoder =
    JD.map
        hostsFromPairs
        (JD.keyValuePairs JD.string)


hostsFromPairs : List ( String, String ) -> List Host
hostsFromPairs pairs =
    List.map (\( k, d ) -> Host (HostKey k) d) pairs |> List.sortBy domain


domain : Host -> String
domain { domain } =
    domain


keyString : Host -> String
keyString { key } =
    case key of
        HostKey s ->
            s
