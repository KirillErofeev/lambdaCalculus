{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List (intersect, isPrefixOf)
import Control.Monad.State.Lazy

newtype Symbol = Symbol { unSymbol :: String } deriving (Eq,Read)
--newtype Symbol = Symbol String deriving (Eq,Show,Read)

instance Show Symbol where
    show (Symbol x) = show x


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

captureSub (SymS x) x' (SymS y) | x == y =  x'
                                | True   =  SymS y

captureSub x x' (AppS t1 t2) = AppS (cs t1) (cs t2) where
    cs = captureSub x x'

captureSub sx@(SymS x) x' term@(LamS sym t) 
                        | sym == x  = term 
                        | True      = LamS sym (captureSub sx x' t)

alpha :: TermS -> TermS
alpha term = evalState (alpha' term ) (symbolStream, [])

alphatest term = runState (alpha' term ) (symbolStream, [])

setBounds bs (s,b) = (s, bs)

isBounded _ (SymS _) = False
isBounded x (AppS t1 t2) = isBounded x t1 || isBounded x t2
isBounded x'@(SymS x) (LamS s t)    | x == s = True
                                    | True   = isBounded x' t

discardBounded term st@((sym:syms),b) | isBounded (SymS sym) term = discardBounded term (syms, b)
                                      | True                      = st 

alpha' :: TermS -> State ([Symbol], [Symbol]) TermS
alpha' (SymS x)   = state $ sub where 
     sub ((s:ss), bounded) | x `elem` bounded = (SymS x, ((s:ss), bounded))
                           | True             = (SymS s, ((ss), bounded))

alpha' (AppS x y) = do
     --bounded <- (snd <$> get)
     x' <- alpha' x
     --bounded' <- (snd <$> get)

     --modify $ setBounds (intersect bounded bounded')

     --bounded <- (snd <$> get)
     y' <- alpha' y
     --bounded' <- (snd <$> get)

     --modify $ setBounds (intersect bounded bounded')

     return $ AppS x' y'

alpha' (LamS x y) = do
    modify $ discardBounded y
    (x':x's) <- (fst <$> get)
    bounded  <- (snd <$> get)
    put (x's, x':bounded)
    r <- alpha' (captureSub (SymS x) (SymS x') y)
    --bounded' <- (snd <$> get)
    --modify $ setBounds (intersect bounded bounded')
    return $ LamS x' r 
-- (1)
-- один шаг редукции, если это возможно. Стратегия вычислений - полная, т.е. редуцируются все возможные редексы.
hasRedex (AppS (LamS _ _) _) = True
hasRedex (AppS (SymS _) t)   = hasRedex t
hasRedex (AppS t1 t2)        =  hasRedex t1 || hasRedex t2
hasRedex (SymS _ )           = False
hasRedex (LamS s t)          = hasRedex t

beta :: TermS -> Maybe TermS
beta (SymS x) = Nothing

beta (LamS s t) | hasRedex t = let
                                   Just beta_t = beta t 
                               in 
                                   Just $ LamS s beta_t
                | True       = Nothing

beta (AppS (SymS _) t2) = Nothing
beta (AppS (LamS sym t) t1) = Just $ captureSub (SymS sym) t1 t
beta t@(AppS t1 t2) | hasRedex t1 = let
                                        Just beta_t1 = beta t1 
                                    in 
                                        Just $ AppS (beta_t1) t2
                    | hasRedex t2 = let
                                        Just beta_t2 = beta t2 
                                    in 
                                        Just $ AppS (t1) (beta_t2)
                    | True        = Nothing



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
          Just y -> lastUnf (n-1) f y

-- выполнять редукцию до конца (но не больше 10000 шагов из-за возможности зависания)
full' a b term = lastUnf 10000 (a term) where
        lastUnf 0 x = x
        lastUnf n x = case b x of
          Nothing -> x
          Just y -> lastUnf (n-1) (a y)

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





test = lam "x" $ lam "x" $ lam "x" $  
    (app 
        (app
            (app
            (lam "b" $ lam "f" $ lam "s" ( app (app (sym "b") (sym "f")) (sym "s"))) 
                 (lam "x" $ lam "y" (sym "x"))) 
                      (lam "x" (sym "x"))) 
                          (lam "x" $ lam "y" (sym "y")))

w   = lam "x" $ app (sym "x") (sym "x")
one = lam "x" $ lam "y" $ app (sym "x") (sym "y")

