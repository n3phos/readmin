module Navbar exposing (navbar)

import Html exposing (..)
import Html.Attributes exposing (..)


navbar : Html msg
navbar =
    nav
        [ class "nav navbar-expand-lg bg-light" ]
        [ ul [ class "navbar-nav mr-auto" ]
            [ li [ class "nav-item" ]
                [ a [ class "nav-link", href "#" ]
                    [ img
                        [ alt "Logo"
                        , title "Home (booking orders statistics)"
                        , src "/resources/DEFAULT/email/swiss_hotels.png"
                        , height 30
                        ]
                        []
                    ]
                ]
            , li [ class "nav-item" ]
                [ a [ class "nav-link", href "#dashboard" ] [ text "Dashboard" ]
                ]
            , li [ class "nav-item" ]
                [ a [ class "nav-link", href "#advertisements" ] [ text "Advertisements" ]
                ]
            , li [ class "nav-item" ]
                [ a [ class "nav-link", href "#promotions" ] [ text "Promotions" ]
                ]
            , li [ class "nav-item dropdown" ]
                [ a [ class "dropdown-toggle nav-link", attribute "data-toggle" "dropdown", href "#" ] [ text "Vouchers", b [ class "caret" ] [] ]
                , ul [ class "dropdown-menu" ]
                    [ li []
                        [ a [ class "nav-link", href "#vouchers/generate" ] [ text "Generate voucher" ]
                        ]
                    , li []
                        [ a [ class "nav-link", href "#vouchers/redeem" ] [ text "Redeem voucher" ]
                        ]
                    , li []
                        [ a [ class "nav-link", href "#vouchers/table" ] [ text "Voucher Table" ]
                        ]
                    ]
                ]
            ]
        ]
