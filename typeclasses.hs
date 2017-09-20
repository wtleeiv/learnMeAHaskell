module Shapes
( Point(..)
, px
, py
, padd
, Shape(..)
, surface
, nudge
, baseCircle
, baseRectangle
) where

import qualified Data.Map as Map

-- define typeclass with: data
data Point = Point Float Float deriving (Show)

px :: Point -> Float
px (Point x _) = x

py :: Point -> Float
py (Point _ y) = y

padd :: Point -> Point -> Point
padd (Point x0 y0) (Point x1 y1) = Point (x0 + x1) (y0 + y1)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle p1 p2) = (abs $ (px p2) - (px p1)) * (abs $ (py p2) - (py p1))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRectangle :: Float -> Float -> Shape
baseRectangle x y = Rectangle (Point 0 0) (Point x y)

nudge :: Float -> Float -> Shape -> Shape
nudge dx dy (Circle p r) = let dp = Point dx dy
                           in Circle (padd p dp) r
nudge dx dy (Rectangle p0 p1) = let dp = Point dx dy
                                in Rectangle (padd p0 dp) (padd p1 dp)


data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
         deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- [minBound..maxBound] :: [Day]

-- type synonym (uses 'type' keyword)
-- data: make new type
-- type: synonym existing type
  -- type String = [Char]
type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (name, number) `elem` book

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int-> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist."
    Just (state, code) -> if state /= Taken
                             then Right code
                             else Left $ "Locker " ++ show lockerNumber ++ " is taken."

lockers :: LockerMap
lockers = Map.fromList
  [(100,(Taken,"ab3s"))
  ,(101,(Free,"xy7r3"))
  ,(103,(Taken,"e3r21"))
  ]
