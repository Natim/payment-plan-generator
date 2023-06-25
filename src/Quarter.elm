module Quarter exposing (Quarter(..), days, deferred_days, fromName, initialDates)

import Days


type Quarter
    = Q1
    | Q2
    | Q3
    | Q4


maxInstallmentsHandled : Int
maxInstallmentsHandled =
    24


initialDates : Quarter -> List String
initialDates quarter =
    case quarter of
        Q1 ->
            [ "2022-01-01", "2022-02-01", "2022-03-01", "2022-03-31" ]

        Q2 ->
            [ "2022-04-01", "2022-05-01", "2022-06-01", "2022-06-30" ]

        Q3 ->
            [ "2022-07-01", "2022-08-01", "2022-09-01", "2022-09-30" ]

        Q4 ->
            [ "2022-10-01", "2021-11-01", "2021-12-01", "2021-12-31" ]


days : Quarter -> List (List Int)
days quarter =
    let
        days_candidates =
            initialDates quarter

        days_candidates_posix =
            List.map Days.toPosix days_candidates
    in
    days_candidates_posix
        |> List.map (Days.timeBetweenPayments maxInstallmentsHandled)


deferred_days : Quarter -> Int -> List (List Int)
deferred_days quarter days_from_start =
    days quarter
        |> List.map
            (\days_candidate ->
                days_from_start :: List.map ((+) days_from_start) days_candidate
            )


fromName : String -> Maybe Quarter
fromName name =
    let
        suffix =
            String.right 2 name
    in
    case suffix of
        "T1" ->
            Just Q1

        "T2" ->
            Just Q2

        "T3" ->
            Just Q3

        "T4" ->
            Just Q4

        _ ->
            Nothing
