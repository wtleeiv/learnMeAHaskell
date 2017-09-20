-- TODO learn higher order functions :)

multThree x y z = x * y * z
multTwoWithNine = multThree 9
multWith27 = multTwoWithNine 3

-- surround infix fns with parens when partially evaluating
divideByTen = (/10)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let
  smallerSorted = quicksort $ filter (<=x) xs
  biggerSorted = quicksort $ filter (>x) xs
  in smallerSorted ++ [x] ++ biggerSorted

-- foldl -> \acc x
-- foldr -> \x acc

elem' a xs = foldl (\acc x -> if acc then acc else a == x) False xs

map' f xs = foldr (\x acc -> f x : acc) [] xs

-- function composition f o g(x)
oddSquareSum = sum . takeWhile (<10000) . filter odd. map (^2) $ [1..]
