module Main where

-- import Prelude hiding ((==))
{- askdajds
ajsdkaljsd
-}

x = 2*2

y = x

g x y = x*x + y + f 2

fac n = if n == 0 then 1 else n * fac (n-1)

iif :: Bool -> t -> t -> t
iif b x y = if b then x else y

iifT = iif True

h0 n = fac n * fac n

h1 :: Integer -> Integer
h1 n = let
  f = fac n
  in f * f

f x = x*x

h2 = f

a = if 0 /= 1 then 2 else 3

b = if not (0 == 1) then 2 else 3

as3SADF_123' = 1

фывфыв = 2

fun x y = 12*x + 34*y

(€) x y = 70*x + y

-- (==) x y = x /= y

r = "asdasd" ++
    "asdasd"

char :: Char
char = 'c'

fff :: Double -> Double -> Double
fff x y = if x < y then x*x else y-x

fib1 n = if n<=2 then 1 else fib1 (n-1) + fib1 (n-2)

fib2 1 = 1
fib2 2 = 1
fib2 n = fib2 (n-1) + fib2 (n-2)

fib2' m = case m of
  1 -> 1
  2 -> 1
  n -> fib2' (n-1) + fib2' (n-2)

fib3 n | n <= 2 = 1
       | otherwise = fib3 (n-1) + fib3 (n-2)

fib3' m = case m of
  n | n <= 2 -> 1
    | otherwise -> fib3' (n-1) + fib3' (n-2)

-- ($) f x = f x
act 0 = 1
act n = act $ n-1

sad = print $ fac $ fib1 $ 3 + 2
-- print (fac (fib1 (3+2)))

-- (.)
das = fac . fib1 $ 5

-- 1,1,2,3,5,8,13,21,34,55,89
printFib = print (fib1 0) >>
           print (fib1 1) >>
           print (fib1 10)

data Tri = On
         | Off
         | Unknown
         deriving (Eq,Read,Show,Ord)

data USD = USD Int deriving (Eq,Show,Read,Ord)

data Point = Point Double Double deriving (Eq,Ord,Read,Show)

toString :: Tri -> String
toString On = "On"
toString Off = "Off"
toString Unknown = "Unknown"

dist :: Point -> Double
dist (Point x y) = sqrt $ x*x + y*y

toRub :: USD -> Int
toRub (USD n) | n < 0 = 0
              | otherwise = 60*n

data Foo = Foo { getInt :: Int }
         | Bar { getInt :: Int }
         | Baz { getPoint :: Point
               , getSecond :: Point }
         | Quux { fooUsd :: USD }
         deriving (Eq,Show,Read,Ord)

-- fooUsd :: Foo -> USD
-- fooUsd (Quux usd) = usd
-- fooUsd _ = error "Wrong!"

foo :: Foo -> Double
foo (Foo f) = fromIntegral f
foo (Bar b) = fromIntegral $ b*2
foo (Baz (Point 0 y) p2) = y*y*y + dist p2
foo (Baz (Point x y) _) = x*x+y*y
-- foo (Quux usd) = fromIntegral $ toRub usd
-- foo x = 1
foo _ = 1

data Pair a = Pair { first :: a
                   , second :: a }

instance Show a => Show (Pair a) where
  show (Pair a b) = "(" ++ show a ++ "," ++ show b ++ ")"

-- (a,b)
-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show,Enum)
data Date = Date { year :: Integer, month :: Int, day :: Int }

{-
isDateValid :: Date -> Bool
isDateValid date = let
  between a b x = a <= x && x <= b
  in    month date == 1 && between 1 31 (day date)
     || month date == 2 && between 1 28 (day date)
     || ...
-}

{-
isDateValid :: Date -> Bool
isDateValid (Date y m d) =
  m == 1 && 1 <= d && d <= 31 ||
  m == 2 && 1 <= d && d <= 28 ||
  ...
-}

setDayIn2017_1_x :: Int -> Date
setDayIn2017_1_x = Date 2017 1

setMonthIn2017_x_1 :: Int -> Date
setMonthIn2017_x_1 month = Date 2017 month 1

setMonthIn2017_x_2 :: Int -> Date
setMonthIn2017_x_2 = \month -> Date 2017 month 1

setMonthYear = \month year -> Date year month 1

twice :: (Int -> Int) -> (Int -> Int)
-- twice f x = f (f x)
twice f = f . f

data Tree a = Leaf { val :: a }
            | Node { left :: Maybe (Tree a)
                   , val :: a
                   , right :: Maybe (Tree a) }

-- data [a] = [] | (:) a [a]
list :: [Int]
-- 1 : 2 : 3 : 4 : 5 : []
list = [1,2,3,4,5]

len :: [a] -> Int
len [] = 0
len (x : xs) = 1 + len xs

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [x] = []
pairs (x : y : zs) = (x,y) : pairs (y : zs)

maxDate :: [Date] -> Maybe Date
maxDate [] = Nothing
maxDate [d] = Just d
maxDate (d : ds) = Just $ let
  lt (Date y1 m1 d1) (Date y2 m2 d2) = y1 <  y2 ||
                                       y1 == y2 && m1 <  m2 ||
                                       y1 == y2 && m1 == m2 && d1 < d2
  Just maxDs = maxDate ds
  in if d `lt` maxDs
     then maxDs
     else d

---------------------------------
-- Классы типов (typeclasses)

class Currency a where
  toRubles :: a -> Int

instance Currency USD where
  toRubles (USD n) = 60*n

data Euro = Euro Int
instance Currency Euro where
  toRubles (Euro n) = 70 * n

instance Show Date where
  show (Date y m d) = show y ++ "-" ++
                      show m ++ "-" ++
                      show d

instance Eq Date where
  (==) d1 d2 =  year d1 ==  year d2 &&
               month d1 == month d2 &&
                 day d1 ==   day d2

instance Ord Date where
  compare d1 d2 = let
    yc = compare (year d1) (year d2)
    mc = compare (month d1) (month d2)
    dc = compare (day d1) (day d2)
    in if yc /= EQ
       then yc
       else if mc /= EQ
            then mc
            else dc

----------------------------------------

list2 :: [Int]
list2 = [6,7,8,9,10]

-- map, filter, foldl, foldr
{-
f = \a b -> a + 10*b
foldr (\a b -> a + 10*b) 0 [1,2,3] ==
(1 `f` (2 `f` (3 `f` 0)))

foldl f 0 [1,2,3] ==
((0 `f` 1) `f` 2) `f` 3
-}

map2 f = foldr (\c ds -> f c : ds) []

main :: IO ()
main = putStrLn "hello world"

