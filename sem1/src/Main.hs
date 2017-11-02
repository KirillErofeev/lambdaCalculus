module Main where

newtype Symbol = Symbol { unSymbol :: String } deriving (Eq,Show,Read)

-- (1)
data TermS = SymS Symbol        -- x
           | LamS Symbol TermS  -- \x -> t
           | AppS TermS TermS   -- t1 t2
           deriving (Eq,Show,Read)

-- (1)
-- переименовать все переменные так, чтобы все они были разными.
alpha :: TermS -> TermS
alpha = error "Implement me!"

-- (1)
-- один шаг редукции, если это возможно. Стратегия вычислений - полная, т.е. редуцируются все возможные редексы.
beta :: TermS -> Maybe TermS
beta = error "Implement me!"

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
-- solve = error "Choose your variant"
-- (1)
solve = Right . full id (beta . alpha) . toTermS
-- (2)
-- solve = Left . full toTermI betaI . toTermS

main :: IO ()
main = do
  s <- read <$> getLine
  print $ solve s
