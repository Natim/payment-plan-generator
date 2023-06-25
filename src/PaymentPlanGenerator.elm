module PaymentPlanGenerator exposing (main)

import Browser
import Date exposing (Date)
import Days
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Input.Float as CurrencyInput
import Interest
import Round
import Task
import Time exposing (Month(..))
import Utils exposing (euros)


type alias Model =
    { purchaseAmount : Maybe Float
    , startDate : Maybe String
    , installmentsCount : Maybe Int
    , paidAmount : Maybe Float
    , paymentPlan : List Days.Installment
    }


type FieldType
    = PurchaseAmount
    | PaidAmount


type Msg
    = DateChanged String
    | InstallmentsCountChanged String
    | CurrencyChanged FieldType (Maybe Float)
    | ReceiveDate Date


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { purchaseAmount = Just 515.5
      , startDate = Nothing
      , installmentsCount = Just 10
      , paidAmount = Just 546.03
      , paymentPlan = []
      }
    , Date.today |> Task.perform ReceiveDate
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                ReceiveDate today ->
                    { model | startDate = Date.toIsoString today |> Just }

                DateChanged value ->
                    { model | startDate = Just value }

                CurrencyChanged PurchaseAmount value ->
                    { model | purchaseAmount = value }

                InstallmentsCountChanged value ->
                    { model | installmentsCount = String.toInt value }

                CurrencyChanged PaidAmount value ->
                    { model | paidAmount = value }
    in
    ( { newModel | paymentPlan = updatePaymentPlan newModel }, Cmd.none )


updatePaymentPlan : Model -> List Days.Installment
updatePaymentPlan model =
    case
        ( ( model.purchaseAmount, model.startDate ), ( model.installmentsCount, model.paidAmount ) )
    of
        ( ( Just purchaseAmount, Just startDate ), ( Just installmentsCount, Just paidAmount ) ) ->
            let
                paymentPlanBuilder =
                    if installmentsCount > 4 then
                        Interest.getCreditPaymentPlan

                    else
                        Days.getPNXPaymentPlan
            in
            paymentPlanBuilder
                installmentsCount
                startDate
                (round <| purchaseAmount * 100)
                (round <| (paidAmount - purchaseAmount) * 100)

        _ ->
            []


annual_interest_rate : Maybe Float -> List Days.Installment -> String
annual_interest_rate maybe_purchaseAmount paymentPlan =
    let
        maybe_taeg =
            maybe_purchaseAmount
                |> Maybe.andThen
                    (\purchaseAmount ->
                        Interest.optimal_interest_rate (round <| purchaseAmount * 100) paymentPlan
                    )
    in
    case maybe_taeg of
        Just taeg ->
            Round.round 2 (taeg * 100)

        Nothing ->
            "-,--"


inputOptions : FieldType -> CurrencyInput.Options Msg
inputOptions field =
    let
        defaultOptions =
            CurrencyInput.defaultOptions (CurrencyChanged field)
    in
    { defaultOptions
        | minValue = Just 0
    }


currencyInput : FieldType -> Maybe Float -> String -> Html Msg
currencyInput field fieldInfo formName =
    div [ class "input-group" ]
        [ CurrencyInput.input
            (inputOptions field)
            [ class "form-control", id formName ]
            fieldInfo
        , span [ class "input-group-addon" ] [ text "€" ]
        ]


viewInstallment : Int -> Days.Installment -> Html Msg
viewInstallment i installment =
    tr [ class "" ]
        [ td []
            [ text <| "E" ++ String.fromInt (i + 1) ]
        , td []
            [ text installment.dueDate ]
        , td []
            [ text <| euros installment.totalAmount ]
        , td []
            [ text <| euros installment.purchaseAmount ]
        , td []
            [ text <| euros installment.customerInterest ]
        ]


viewPaymentPlan : List Days.Installment -> Html Msg
viewPaymentPlan paymentPlan =
    case paymentPlan of
        [] ->
            text ""

        _ ->
            let
                feeTitle =
                    if List.length paymentPlan > 4 then
                        "Intérêts"

                    else
                        "Frais client"
            in
            table [ class "table table-condensed" ]
                [ thead []
                    [ tr []
                        [ th []
                            [ text "#" ]
                        , th []
                            [ text "Date" ]
                        , th []
                            [ text "Montant" ]
                        , th []
                            [ text "Capital" ]
                        , th []
                            [ text feeTitle ]
                        ]
                    ]
                , List.indexedMap viewInstallment paymentPlan |> tbody []
                ]


view : Model -> Html Msg
view { startDate, purchaseAmount, installmentsCount, paidAmount, paymentPlan } =
    div []
        [ div [ class "col-sm-6" ]
            [ div [ class "form-group col-sm-6" ]
                [ label [ for "purchase_amount", class "col-sm-6 control-label" ] [ text "Montant de l'achat" ]
                , div [ class "col-sm-6" ]
                    [ currencyInput PurchaseAmount purchaseAmount "purchase_amount" ]
                ]
            , div [ class "form-group col-sm-6" ]
                [ label [ for "paid_amount", class "col-sm-6 control-label" ] [ text "Montant payé" ]
                , div [ class "col-sm-6" ]
                    [ currencyInput PaidAmount paidAmount "paid_amount"
                    ]
                ]
            , div [ class "form-group col-sm-6" ]
                [ label [ for "start_date", class "col-sm-6 control-label" ] [ text "Date d'achat" ]
                , div [ class "col-sm-6" ]
                    [ input
                        [ type_ "date"
                        , class "form-control"
                        , style "padding-top" "0"
                        , id "start_date"
                        , value <| Maybe.withDefault "" startDate
                        , onInput <| DateChanged
                        ]
                        []
                    ]
                ]
            , div [ class "form-group col-sm-6" ]
                [ label [ for "installments_count", class "col-sm-6 control-label" ] [ text "Nombre d'échéances" ]
                , div [ class "col-sm-6" ]
                    [ div [ class "input-group" ]
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , id "installments_count"
                            , value <| String.fromInt <| Maybe.withDefault 0 installmentsCount
                            , onInput <| InstallmentsCountChanged
                            ]
                            []
                        , span [ class "input-group-addon" ] [ text "fois" ]
                        ]
                    ]
                ]
            ]
        , div [ class "col-sm-6" ]
            [ p []
                [ h1 [ class "text-center" ]
                    [ text "Votre TAEG pour ce paiement est de "
                    , text <| annual_interest_rate purchaseAmount paymentPlan
                    , text "%"
                    ]
                ]
            ]
        , div [ class "col-sm-6" ]
            [ viewPaymentPlan paymentPlan
            ]
        ]
