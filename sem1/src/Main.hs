{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List (isPrefixOf)
import Control.Monad.State.Lazy

--newtype Symbol = Symbol { unSymbol :: String } deriving (Eq,Show,Read)
newtype Symbol = Symbol String deriving (Eq,Show,Read)

-- (1)
data TermS = SymS Symbol        -- x
           | LamS Symbol TermS  -- \x -> t
           | AppS TermS TermS   -- t1 t2
           deriving (Eq,Show,Read)

sym x     = SymS (Symbol x)
lam x t   = LamS (Symbol x) t
app t1 t2 = AppS t1 t2

-- (1)
-- переименовать все переменные так, чтобы все они были разными.
symbolStream = map Symbol $ iterate succS "a" where
    succS l@(x:xs) | x < 'z'                    = succ x : xs
                   | isPrefixOf xs (repeat 'z') = 'a' : take (length l) (repeat 'a')
                   | True                       = 'a' : succS xs

captureSub x x' (SymS y) | x == y = SymS x'
                         | True   = SymS y

captureSub x x' (AppS t1 t2) = AppS (cs t1) (cs t2) where
    cs = captureSub x x'

captureSub x x' term@(LamS sym t) | sym == x = term 
                                  | True     = LamS sym (captureSub x x' t)

alpha :: TermS -> TermS
alpha term = evalState (alpha' term ) (symbolStream, [])

alphatest term = runState (alpha' term ) (symbolStream, [])

alpha' :: TermS -> State ([Symbol], [Symbol]) TermS
alpha' (SymS x)   = state $ sub where 
     sub ((s:ss), bounded) | x `elem` bounded = (SymS x, ((s:ss), bounded))
                           | True             = (SymS s, ((ss), bounded))

alpha' (AppS x y) = do
     x' <- alpha' x
     y' <- alpha' y
     return $ AppS x' y'

alpha' (LamS x y) = do
    (x':x's) <- (fst <$> get)
    bounded  <- (snd <$> get)
    put (x's, x':bounded)
    r <- alpha' (captureSub x x' y)
    return $ LamS x' r 

test = lam "x" $ lam "x" $ lam "x" $ app 
    (app 
        (lam "b" $ lam "f" $ lam "s" ( app (sym "b") (app (sym "f") (sym "s")))) 
          (lam "x" $ lam "y" (sym "x"))) 
                   (app 
                       (lam "x" (sym "x")) (lam "x" $ lam "y" (sym "y")))


-- (1)
-- один шаг редукции, если это возможно. Стратегия вычислений - полная, т.е. редуцируются все возможные редексы.
hasRedex (AppS _ _) = True
hasRedex (SymS _ )  = False
hasRedex (LamS s t) = hasRedex t

beta :: TermS -> Maybe TermS
beta (SymS x) = Nothing

beta (LamS s t) | hasRedex t = LamS s (beta t)
                | True       = Nothing

--beta (Apps t1 t2) = 



-- (2)
data TermI = SymI Int
           | LamI TermI
           | AppI TermI TermI
           deriving (Eq,Show,Read)

-- (2)
-- перевод выражения в TermI
toTermI :: TermS -> TermI
toTermI = error "Implement me!"

-- (2)
-- шаг редукции
betaI :: TermI -> Maybe TermI
betaI = error "Implement me!"

-- выполнять редукцию до конца (но не больше 10000 шагов из-за возможности зависания)
full :: (TermS -> a) -> (a -> Maybe a) -> TermS -> a
full a b term = lastUnf 10000 b (a term)
  where lastUnf :: Int -> (a -> Maybe a) -> a -> a
        lastUnf 0 _ x = x
        lastUnf n f x = case f x of
          Nothing -> x
          Just y -> lastUnf (n-1) f (a y)

data TermP = TermP TermS
           -- (3)
           | Boolean Bool
           | Iff TermP TermP TermP
           | Not TermP
           | And TermP TermP
           | Or TermP TermP
           -- (4)
           | Natural Int
           | Plus TermP TermP
           | Mult TermP TermP
           -- (4*) +10%
           | Minus TermP TermP
           | Divide TermP TermP
           -- (5*) +50%
           | Y TermP
           -- (5**) +50%
           -- mutually recursive
           -- (6)
           | Pair TermP TermP
           | Fst TermP
           | Snd TermP
           -- (7)
           | Cons TermP TermP
           | Nil
           | IsNil TermP
           | Head TermP
           | Tail TermP
           deriving (Eq,Show,Read)

toTermS :: TermP -> TermS
toTermS = error "Implement me!"

solve :: TermP -> Either TermI TermS
solve = error "Choose your variant"
-- (1)
-- solve = Right . full alpha beta . toTermS
-- (2)
-- solve = Left . full toTermI betaI . toTermS

main :: IO ()
main = do
  s <- read <$> getLine
  print $ solve s
