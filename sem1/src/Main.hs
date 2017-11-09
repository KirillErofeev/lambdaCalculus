{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List (intersect, isPrefixOf)
import Control.Monad.State.Lazy

import Data.Numbers.Primes

import Types
import LambdaStream
import Combinators
import Solver
import Reduction
import Tests

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

--true, app, iff и пр. определены в Combinators.hs
toTermS :: TermP -> TermS
toTermS (Boolean True)  = true
toTermS (Boolean False) = false
toTermS (Iff bt t0 t1)  = apps $ iff : fmap toTermS [bt, t0, t1]
toTermS (Not t)         = apps $ iff : [toTermS t, false, true]
toTermS (And t t')      = apps $ iff : fmap toTermS [t, t'] ++ [false]
toTermS (Or t0 t1)      = let 
                              t  = toTermS t0 
                              t' = toTermS t1
                          in 
                              apps $ iff : t : true : t' : []
toTermS (Pair t t')      = app3 pair (toTermS t) (toTermS t')
toTermS (Fst t)          = app fst' (toTermS t) 
toTermS (Snd t)          = app snd' (toTermS t) 


solve :: TermP -> Either TermI TermS
-- solve = error "Choose your variant"
-- (1)
solve = Right . full id (beta . alpha) . toTermS
-- (2)
-- solve = Left . full toTermI betaI . toTermS

main :: IO ()
main = do
  s <- read <$> getLine
  print $ solve s

--alpha' (AppS x y) = do
--     bounded <- (snd <$> get)
--     x' <- alpha' x
--     modify $ setBounds bounded
--     y' <- alpha' y
--     modify $ setBounds bounded
--     return $ AppS x' y'
