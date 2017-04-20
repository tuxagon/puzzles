module Permutations exposing (..)

import Html exposing (Html, text)


main : Html a
main =
    permute [ 1, 2, 3, 4, 5 ]
        |> toString
        |> text



{- }

   The permutations are built up by taking each item in the list and calling `permute`
   on the remaining list. So

       [1,2,3] =
           1 :: permute [2,3] =
               2 :: permute [3] =
                   3 :: []
               3 :: permute [2] =
                   2 :: []
           2 :: permute [1,3]
           3 :: permute [1,2]

-}


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
