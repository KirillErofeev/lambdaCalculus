{-# LANGUAGE NoMonomorphismRestriction #-}

module Types where 

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

app3 t1 t2 t3    = AppS (AppS t1 t2) t3
app4 t1 t2 t3 t4 = AppS (app3 t1 t2 t3) t4

apps = foldl1 AppS

