module Reduction where

import Data.List (isPrefixOf)
import Control.Monad.State.Lazy

import Types 

-- (1)
-- переименовать все переменные так, чтобы все они были разными.

--бесконечный список имен лямбд
symbolStream = map Symbol $ iterate succS "a" where
    succS l@(x:xs) | x < 'z'                    = succ x : xs
                   | isPrefixOf xs (repeat 'z') = 'a' : take (length l) (repeat 'a')
                   | True                       = 'a' : succS xs

--синтаксическая переподстановка имен
captureSub (SymS x) x' (SymS y) | x == y =  x'
                                | True   =  SymS y

captureSub x x' (AppS t1 t2) = AppS (cs t1) (cs t2) where
    cs = captureSub x x'

captureSub sx@(SymS x) x' term@(LamS sym t) 
                        | sym == x  = term 
                        | True      = LamS sym (captureSub sx x' t)


alphatest term = runState (alpha' term ) (symbolStream, [])


setBounds bs (s,b) = (s, bs)

--провера связана ли переменная какой-нибудь лямбдой
isBounded _ (SymS _) = False
isBounded x (AppS t1 t2) = isBounded x t1 || isBounded x t2
isBounded x'@(SymS x) (LamS s t)    | x == s = True
                                    | True   = isBounded x' t

--отбрасываем связанные имена переменные из бесконечного списка имен переменных
discardBounded term st@((sym:syms),b) | isBounded (SymS sym) term = discardBounded term (syms, b)
                                      | True                      = st 

--запуск вычислений в монаде State
alpha :: TermS -> TermS
alpha term = evalState (alpha' term ) (symbolStream, [])

--Если символ связан он не меняется, если не связан меняется на следующий символ из списка
alpha' :: TermS -> State ([Symbol], [Symbol]) TermS
alpha' (SymS x)   = state $ sub where 
     sub ((s:ss), bounded) | x `elem` bounded = (SymS x, ((s:ss), bounded))
                           | True             = (SymS s, ((ss), bounded))

--альфа от применения - просто применение двух альф
alpha' (AppS x y) = do
     x' <- alpha' x
     y' <- alpha' y
     return $ AppS x' y'

-- альфа от абстракции
alpha' (LamS x y) = do
    modify $ discardBounded y -- отбрасываем из бесконечного спискам переменные, связанные в обрабатываемом терме
    (x':x's) <- (fst <$> get) -- достаем первую не связанную переменную
    bounded  <- (snd <$> get) -- достаем список связанных
    put (x's, x':bounded)     -- добавляем связанную на этом шаге переменную в списко связанных
    r <- alpha' (captureSub (SymS x) (SymS x') y) -- делаем синтаксическую замену
    bounded' <- (snd <$> get) 
    modify $ setBounds bounded-- возвращаем список связанных переменных в состояние, в котором он был до заходу в эту лямбду
    return $ LamS x' r 

-- (1)
-- один шаг редукции, если это возможно. Стратегия вычислений - полная, т.е. редуцируются все возможные редексы.


-- проверяем есть ли в терме редекс
hasRedex (AppS (LamS _ _) _) = True
hasRedex (AppS (SymS _) t)   = hasRedex t
hasRedex (AppS t1 t2)        = hasRedex t1 || hasRedex t2
hasRedex (SymS _ )           = False
hasRedex (LamS s t)          = hasRedex t

beta :: TermS -> Maybe TermS
beta (SymS x) = Nothing

beta (LamS s t) | hasRedex t = Just $ LamS s (unsafeBeta t)
                | True       = Nothing

beta (AppS (SymS t) t') | hasRedex t' = Just (AppS (SymS t) (unsafeBeta t'))
                        | True        = Nothing

beta (AppS (LamS sym t) t1) = Just $ captureSub (SymS sym) t1 t

beta t@(AppS t1 t2) | hasRedex t1 = Just $ AppS (unsafeBeta t1) t2
                    | hasRedex t2 = Just $ AppS (t1) (unsafeBeta t2)
                    | True        = Nothing

--распаковываем результат бета преобразования из Maybe, когда уверенны, что он там есть
unsafeBeta t = let Just t' = beta t in t'


-- выполнять редукцию до конца (но не больше 10000 шагов из-за возможности зависания)
full' a b term = lastUnf 10000 (a term) where
        lastUnf 0 x = x
        lastUnf n x = case b x of
          Nothing -> x
          Just y -> lastUnf (n-1) (a y)

full'' = full' alpha beta

-------------
eq t t' = toN t == toN t' where
     toN t = alpha $ full'' t

eqLN t t' = t == toN t' where
     toN t = alpha $ full'' t

-- выполнять редукцию до конца (но не больше 10000 шагов из-за возможности зависания)
full :: (TermS -> a) -> (a -> Maybe a) -> TermS -> a
full a b term = lastUnf 10000 b (a term)
  where lastUnf :: Int -> (a -> Maybe a) -> a -> a
        lastUnf 0 _ x = x
        lastUnf n f x = case f x of
          Nothing -> x
          Just y -> lastUnf (n-1) f y

