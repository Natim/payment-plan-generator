module Days exposing (Installment, buildPlanDays, getPNXPaymentPlan, getPurchaseAmountPhasing, schedulePaymentDates, timeBetweenPayments, toPosix, toString)

import Iso8601
import Time exposing (Month(..), Posix, utc)
import Time.Extra as TE exposing (Interval(..))


type alias Installment =
    { dueDate : String
    , totalAmount : Int
    , purchaseAmount : Int
    , customerInterest : Int
    }


toPosix : String -> Posix
toPosix =
    Iso8601.toTime
        >> Result.withDefault (Time.millisToPosix 0)


toString : Posix -> String
toString =
    Iso8601.fromTime >> String.left 10


schedulePaymentDates : Int -> Posix -> List Posix
schedulePaymentDates installments_count starting_date =
    List.range 0 (installments_count - 1)
        |> List.map (\i -> TE.add Month i utc starting_date)


timeBetweenPayments : Int -> Posix -> List Int
timeBetweenPayments installmentsCount starting_date =
    schedulePaymentDates installmentsCount starting_date
        |> List.drop 1
        |> List.map (\end_date -> TE.diff Day utc starting_date end_date)


buildPlanDays : Int -> List Int -> List Int
buildPlanDays installments_count days =
    let
        durations =
            days
                |> List.take (installments_count - 1)
    in
    if installments_count > 4 then
        List.map ((+) -3) durations

    else
        durations


getPurchaseAmountPhasing : Int -> Int -> List Int
getPurchaseAmountPhasing installmentsCount purchaseAmount =
    let
        n =
            toFloat installmentsCount

        amount =
            toFloat purchaseAmount

        alpha =
            1 / n

        after =
            floor <| amount * (1 - alpha) / (n - 1)

        first =
            purchaseAmount - ((installmentsCount - 1) * after)
    in
    first :: List.repeat (installmentsCount - 1) after


getPNXPaymentPlan : Int -> String -> Int -> Int -> List Installment
getPNXPaymentPlan installmentsCount startingDate purchaseAmount customerFee =
    let
        dates =
            List.map toString <| schedulePaymentDates installmentsCount (toPosix startingDate)

        purchaseAmountPhasing =
            getPurchaseAmountPhasing installmentsCount purchaseAmount

        totalAmountPhasing =
            case purchaseAmountPhasing of
                [] ->
                    []

                x :: xs ->
                    (x + customerFee) :: xs

        customerFeePhasing =
            customerFee :: List.repeat (installmentsCount - 1) 0
    in
    List.map4 Installment
        dates
        totalAmountPhasing
        purchaseAmountPhasing
        customerFeePhasing
