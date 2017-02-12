module Auxiliar exposing (mapFilter, removeLast, sum, getFromList)

mapFilter : (a -> a) -> ( a -> Bool) -> List a -> List a
mapFilter m f l =
  case l of
    [] -> []
    (x::xs) ->
      if f x then
        (m x)::(mapFilter m f xs)
      else
        x::(mapFilter m f xs)

getFromList : Int -> List a -> Maybe a
getFromList n l =
  case l of
    [] -> Nothing
    (x::xs) ->
    if n < 0 then
      Nothing
    else if n == 0 then
      Just x
    else
      getFromList (n-1) xs

getLast : List a -> Maybe a
getLast l =
  case List.take 1 (List.reverse l) of
    [x] -> Just x
    _ -> Nothing


removeLast : List a -> List a
removeLast l = List.take ((List.length l) - 1) l


sum : (Int, Int) -> (Int, Int) -> (Int, Int)
sum (x, y) (w, z) = (x+w, y+z)
