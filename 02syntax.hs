lucky :: (Integral n) => n -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky _ = "Sorry, no luck this time"

sayMe :: (Integral n) => n -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"
sayMe 5 = "Five"
sayMe _ = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVect2 :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVect2 a b = (fst a + fst b, snd a + snd b)

first :: (a,b,c) -> a
first (x,_,_) = x
second :: (a,b,c) -> b
second (_,y,_) = y
third :: (a,b,c) -> c
third (_,_,z) = z

head' :: [a] -> a
head' [] = error "cannot call head' on empty list"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

firstLetter :: String -> String
firstLetter all@(x:_) = "The first letter in " ++ all ++ " is " ++ [x]

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a < b = LT
  | a == b = EQ
  | otherwise = GT

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= thin = "thin"
  | bmi <= normal = "normal"
  | bmi <= thick = "thick"
  | otherwise = "fat af"
  where bmi = weight / (height ^ 2)
        (thin,normal,thick) = (18.5,25.0,30.0)

calcBMIs :: (RealFloat a) => [(a,a)] -> [a]
calcBMIs xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / (height ^ 2)

cylinderSurfaceArea :: (RealFloat a) => a -> a -> a
cylinderSurfaceArea radius height =
  let sideArea = 2 * pi * radius * height
      topArea = pi * radius ^ 2
  in sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = case xs of [] -> "empty"
                             [x] -> "single"
                             xs -> "many"
