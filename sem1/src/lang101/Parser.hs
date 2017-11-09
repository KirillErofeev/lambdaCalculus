module Parser where

newtype MyList a = MyList [a]
instance Functor MyList where
  fmap f (MyList as) = MyList $ go as
    where go [] = []
          go (b:bs) = f b : go bs
{-
instance Functor MyList where
  fmap f (MyList xs) = MyList $ fmap f xs
-}

newtype MyEither a b = MyEither (Either a b)
instance Functor (MyEither a) where
  fmap f (MyEither (Left a)) = MyEither $ Left a
  fmap f (MyEither (Right b)) = MyEither $ Right $ f b

newtype MyPair a b = MyPair (a,b)
instance Functor (MyPair a) where
  fmap f (MyPair (a,b)) = MyPair (a, f b)

newtype MyMaybe a = MyMaybe (Maybe a)
instance Functor MyMaybe where
  fmap _ (MyMaybe Nothing) = MyMaybe Nothing
  fmap f (MyMaybe (Just a)) = MyMaybe $ Just $ f a

newtype MyFun a b = MyFun (a -> b)
instance Functor (MyFun r) where
  fmap f (MyFun g) = MyFun $ f . g
  --                         \x -> f (g x)


------ fmap f . fmap g = fmap (f.g)
------ fmap id = id



instance Applicative MyList where
  pure x = MyList [x]
  MyList fs <*> MyList xs = MyList $ fs <*> xs



----- pure id <*> v = v
----- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
----- pure f <*> pure x = pure (f x)
----- u <*> pure y = pure ($ y) <*> u




newtype Parser a = Parser { parse :: String -> [(String, a)]}

instance Functor Parser where
  fmap f (Parser g) = Parser $ fmap (fmap (fmap f)) g
                      -- \s -> map (\(t,a) -> (t,f a)) $ g s
-- f <$> x = fmap f x

instance Applicative Parser where
  pure x = Parser $ pure (pure (pure x))
           -- \s -> [(s,x)]
  Parser pf <*> Parser px = Parser $ \s -> let
    frs = pf s
    in concatMap (\(t,f) -> map (\(rest,x) -> (rest, f x)) $ px t)
       frs

-- pure id <*> v =
-- Parser (\s -> [(s,x)]) <*> Parser pv =
-- Parser $ \s -> let { frs = pf s } in ... =
-- Parser $ \s -> let { frs = [(s,id)]} in ... =
-- Parser $ \s -> concatMap (\(t,f) -> map (...) $ pv t) [(s,id)] =
-- Parser $ \s -> map (\(rest,x) -> (rest, id x)) $ pv s =
-- Parser $ \s -> map (\(rest,x) -> (rest, id x)) $ pv s =
-- Parser $ \s -> map (\(rest,x) -> (rest, x)) $ pv s =
-- Parser $ \s -> fmap id $ pv s =
-- Parser $ \s -> id $ pv s =
-- Parser $ \s -> pv s =
-- Parser pv =
-- v

-- pure ($ y) <*> u =
-- pure (\x -> x y) <*> Parser pu =
-- Parser (\s -> [(s, \x -> x y)]) <*> Parser pu =
-- Parser $ \s -> let { frs = pf s } in ... =
-- Parser $ \s -> map (\(rest,x) -> (rest, (\x -> x y) x)) $ pu s =
-- Parser $ \s -> map (\(rest,x) -> (rest, x y)) $ pu s =
--
-- Parser $ \s -> map (\(t,f) -> (t, f y)) $ pu s =
-- Parser $ \s -> concatMap (\(t,f) -> [(t, f y)]) $ pu s =
-- Parser $ \s -> concatMap (\(t,f) -> map (...) [(s,y)]) =
-- Parser $ \s -> let { frs = pu s }
--                in concatMap (\(t,f) -> map (...) $ pv t) $ pu s=
-- Parser $ \s -> let { frs = pf s } in ... =
-- Parser pu <*> Parser $ \s -> [(s, y)] =
-- u <*> pure y


