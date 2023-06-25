module Utils exposing (euros)

import Round


euros : Int -> String
euros cents =
    let
        amount =
            toFloat cents / 100
    in
    (Round.round 2 amount ++ "€")
        |> String.replace "." ","
        |> String.replace ",00" ""
