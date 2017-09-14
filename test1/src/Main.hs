module Main where

-- import Prelude hiding ((==))
{- askdajds
ajsdkaljsd
-}

x = 2*2

y = x

g x y = x*x + y + f 2

fac n = if n == 0 then 1 else n * fac (n-1)

iif b x y = if b then x else y

iifT = iif True

h0 n = fac n * fac n

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


main :: IO ()
main = putStrLn "hello world"
