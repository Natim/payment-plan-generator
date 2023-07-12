module Interest exposing (getCreditPaymentPlan, optimal_interest_rate, rateWithinDays, show)

import Days exposing (Installment, timeBetweenPayments)
import Newton
import Quarter
import Round


rateWithinDays : Float -> Int -> Float
rateWithinDays rate days =
    (1 + rate) ^ (toFloat days / 365) - 1


getPnxMaxBPS : Int -> Int -> List Int -> String
getPnxMaxBPS installments_count rate planDurations =
    let
        sum =
            planDurations
                |> Days.buildPlanDays (installments_count + 1)
                |> List.map (\d -> 1 / (1 + toFloat rate / 10000) ^ (toFloat d / 365))
                |> List.sum

        rounded_value =
            ((toFloat installments_count / sum - 1) * 10000)
                |> Round.floor 0
    in
    rounded_value


alpha : Int -> List Int -> Float
alpha rate planDuration =
    case planDuration |> List.reverse of
        [] ->
            1.0

        days :: remainingDuration ->
            (rateWithinDays (toFloat rate / 10000) days + 1) * (alpha rate <| List.reverse remainingDuration) + 1


beta : Int -> List Int -> Float
beta rate planDuration =
    case planDuration |> List.reverse of
        [] ->
            0.0

        days :: remainingDuration ->
            let
                monthlyRate =
                    rateWithinDays (toFloat rate / 10000) days
            in
            (monthlyRate + 1) * (beta rate <| List.reverse remainingDuration) - monthlyRate


installmentAmount : Int -> List Int -> Float
installmentAmount rate planDuration =
    (1 - beta rate planDuration) / alpha rate planDuration


getCreditMaxBPS : Int -> Int -> List Int -> String
getCreditMaxBPS installments_count rate planDurations =
    let
        daysBetweenPayments =
            planDurations
                |> Days.buildPlanDays installments_count
                |> List.foldl (\nbDaysFromStartDate acc -> nbDaysFromStartDate - List.sum acc :: acc) []
                |> List.reverse

        maxBPS =
            toFloat installments_count * installmentAmount rate daysBetweenPayments - 1
    in
    Round.floor 0 (maxBPS * 10000)


show : Int -> Int -> Maybe Int -> String -> String
show installments_count deferred_days maybe_rate publicationName =
    let
        maybe_quarter =
            Quarter.fromName publicationName
    in
    case ( maybe_rate, maybe_quarter ) of
        ( Just rate, Just quarter ) ->
            let
                rounded_value =
                    if installments_count > 4 then
                        Quarter.days quarter
                            |> List.map (getCreditMaxBPS installments_count rate)
                            |> List.minimum

                    else
                        Quarter.deferred_days quarter deferred_days
                            |> List.map (getPnxMaxBPS installments_count rate)
                            |> List.minimum
            in
            rounded_value
                |> Maybe.map (\bps -> bps ++ " bps")
                |> Maybe.withDefault "--- bps"

        ( _, _ ) ->
            "--- bps"


optimal_interest_rate : Int -> List Installment -> Maybe Float
optimal_interest_rate purchaseAmount paymentPlan =
    case paymentPlan of
        [] ->
            Nothing

        firstInstallment :: dueInstallments ->
            let
                startDate =
                    firstInstallment.dueDate
                        |> Days.toPosix

                installmentsCount =
                    List.length paymentPlan

                planDurations =
                    startDate
                        |> Days.timeBetweenPayments installmentsCount
                        |> Days.buildPlanDays installmentsCount

                loanAmount =
                    purchaseAmount - firstInstallment.totalAmount

                f_sum x =
                    List.map2 (\d installment -> toFloat installment.totalAmount * (1 / (1 + x)) ^ (toFloat d / 365)) planDurations dueInstallments
                        |> List.sum

                f x =
                    toFloat loanAmount - f_sum x

                maybe_taeg =
                    Newton.optimize f
            in
            maybe_taeg
                |> Maybe.andThen
                    (\taeg ->
                        if taeg > 10 || taeg < 0 then
                            Nothing

                        else
                            Just taeg
                    )


getCreditPaymentPlan : Int -> String -> Int -> Int -> List Installment
getCreditPaymentPlan installmentsCount startingDate purchaseAmount customerFee =
    let
        dates =
            List.map Days.toString <| Days.schedulePaymentDates installmentsCount (Days.toPosix startingDate)

        totalAmountPhasing =
            Days.getPurchaseAmountPhasing installmentsCount (purchaseAmount + customerFee)

        zeros =
            List.repeat installmentsCount 0

        daysBetweenPayments =
            startingDate
                |> Days.toPosix
                |> Days.timeBetweenPayments installmentsCount
                |> Days.buildPlanDays installmentsCount
                |> List.foldl (\nbDaysFromStartDate acc -> nbDaysFromStartDate - List.sum acc :: acc) []
                |> List.reverse

        maybe_taeg =
            List.map4 Installment
                dates
                totalAmountPhasing
                zeros
                zeros
                |> optimal_interest_rate purchaseAmount
    in
    case maybe_taeg of
        Nothing ->
            []

        Just taeg ->
            let
                ( _, purchaseAmountPhasing, interestPhasing ) =
                    List.map2 Tuple.pair totalAmountPhasing (0 :: daysBetweenPayments)
                        |> List.foldl
                            (\( totalAmount, days ) ( capitalLeftToPay, purchaseAcc, interestAcc ) ->
                                let
                                    monthlyRate =
                                        rateWithinDays taeg days

                                    interest =
                                        if days == 0 then
                                            0

                                        else
                                            (capitalLeftToPay * monthlyRate)
                                                |> round

                                    amount =
                                        totalAmount - interest
                                in
                                ( capitalLeftToPay - toFloat amount, amount :: purchaseAcc, interest :: interestAcc )
                            )
                            ( toFloat purchaseAmount, [], [] )
            in
            List.map4 Installment
                dates
                totalAmountPhasing
                (purchaseAmountPhasing |> List.reverse)
                (interestPhasing |> List.reverse)
