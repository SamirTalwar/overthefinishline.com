module App.List exposing (..)

slideThrough : ( List a ->a -> List a -> b) -> List a -> List b
slideThrough f list =
  let slideThrough' f before after =
    case after of
      [] -> []
      (x :: xs) -> f before x xs :: slideThrough' f (before ++ [x]) xs
  in slideThrough' f [] list
