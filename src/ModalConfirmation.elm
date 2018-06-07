module ModalConfirmation exposing (view, ModalConfirmation(..))

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Advertisements.Model exposing (AdvertisementId)
import Msgs


type ModalConfirmation
    = ModalConfirmationNone
    | ModalConfirmationDeleteAdvertisement AdvertisementId String


view : ModalConfirmation -> Html Msgs.Msg
view content =
    case content of
        ModalConfirmationNone ->
            Html.text ""

        ModalConfirmationDeleteAdvertisement adId title ->
            div [ class "modal-container" ]
                [ div [ class "modal-body" ]
                    [ div [] [ (Html.text <| "Delete advertisement “" ++ title ++ "”?") ]
                    , button
                        [ class "btn btn-danger", onClick <| Msgs.DeleteAdvertisement adId ]
                        [ text "Delete" ]
                    , button
                        [ class "btn btn-secondary", onClick Msgs.CloseModalConfirmation ]
                        [ text "Cancel" ]
                    ]
                , div [ class "modal-overlay", onClick Msgs.CloseModalConfirmation ] []
                ]
