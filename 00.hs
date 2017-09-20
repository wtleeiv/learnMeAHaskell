doubleMe x = x*2

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x < 100
  then doubleMe x
  else x

mario = "It's a me, a Mario!"

lostNumbers = [4,8,15,16,42]
-- append
foundNumbers = lostNumbers ++ [5]
-- cons
newNumbers = 5:lostNumbers
-- !! index
yee = newNumbers !! 2 == 8

listy = [1,2,3,4,5]

-- length
-- null
-- reverse
-- take n list -> get first n in list
-- drop n list -> remove first n in list

-- many fns act on lists
-- minimum, maximum, sum, pruduct, etc...

a = 4 `elem` listy -- True
b = 6 `elem` listy -- False

-- ranges (are inclusive)
oneToTwenty = [1..20]
allLowers = ['a'..'z']
kToN = ['K'..'N']
-- steps
erryOtherLetter = ['a','c'..'z']
odds = [1,3..20]
-- infinite lists (are chillin', thanks lazy eval :)
c = take 7 [360,720..]
-- cycle, repeat return infinite lists

-- list comprehensions (hella math style)
d = [x*2 | x <- [1..10], x*x < 17 ]
  -- x*2 for x in 1..10, where x*x > 17

-- use _ for vars we don't care about
length' l = sum [1 | _ <- l]
onlyUpper l = [c | c <- l, c `elem` ['A'..'Z']]


-- tuples
  -- constant num, diff types ok
e = fst (1,2)
f = snd (1,2)
g = zip [1..] ["one","two","three","four","five"]

rightTriangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], c^2 == a^2 + b^2]
