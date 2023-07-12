module Commission exposing (getWeeklyPaymentPlan)

import Days exposing (Installment)
import Time exposing (Posix, utc)
import Time.Extra as TE exposing (Interval(..))


scheduleWeeklyPaymentDates : Int -> Posix -> List Posix
scheduleWeeklyPaymentDates installments_count starting_date =
    List.range 0 installments_count
        |> List.map (\i -> TE.add Week i utc starting_date)


getWeeklyPaymentPlan : Int -> String -> Int -> Int -> List Installment
getWeeklyPaymentPlan installmentsCount startingDate purchaseAmount customerFee =
    let
        dates =
            List.map Days.toString <| scheduleWeeklyPaymentDates installmentsCount (Days.toPosix startingDate)

        totalPayAmount =
            (toFloat (purchaseAmount + customerFee) / toFloat installmentsCount)
                |> round

        totalAmountPhasing =
            List.repeat installmentsCount totalPayAmount

        capitalAmount =
            (toFloat purchaseAmount / toFloat installmentsCount) |> round

        commissionAmount =
            (toFloat customerFee / toFloat installmentsCount) |> round
    in
    List.map4 Installment
        dates
        totalAmountPhasing
        (List.repeat installmentsCount capitalAmount)
        (List.repeat installmentsCount commissionAmount)
