module Main exposing (..)

import Html exposing (..)
import String


main =
    permute [ 1, 2, 3 ]
        |> toString
        |> text


permute : List a -> List (List a)
permute xs =
    let
        permute_ : List a -> a -> List (List a)
        permute_ xs_ x =
            xs
                |> List.filter ((/=) x)
                |> permute
                |> List.map ((::) x)
    in
        case xs of
            [] ->
                [ [] ]

            x :: [] ->
                [ [ x ] ]

            xs_ ->
                List.foldr (++) [] (List.map (permute_ xs_) xs_)
