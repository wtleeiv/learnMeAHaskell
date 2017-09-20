-- :t var gets type of var
-- type declarations
onlyUpper :: [Char] -> [Char]
onlyUpper l = [c | c <- l, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- common types
  -- Int: bounded
  -- Integer: unbounded (hella big nums)
  -- Float
  -- Double
  -- Bool
  -- Char

-- type variables
  -- :t head
  -- head :: [a] -> a

  -- :t fst
  -- fst :: (a, b) -> a

-- type classes
  -- :t (==)
  -- (==) :: Eq a => a -> a -> Bool
    -- a must be equatable
  -- Eq : equality
  -- Ord : orderable
  -- Show : string representation
  -- Read : read from string to type
    -- :t read
    -- read :: Read a => String -> a
  -- Enum : enumeratable
  -- Bounded : has a max and min
  -- Num : is a number (Int, Integer, Float, Double)
  -- Integral : (Int, Integer)
  -- Floating : (Float, Double)

