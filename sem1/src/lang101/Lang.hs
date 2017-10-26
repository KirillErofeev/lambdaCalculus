{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.Maybe

type Name = String

data Simple = Const Name
            | Var Name
            | Any
            deriving (Eq,Show,Read)

data PrologTerm = Sim Simple
                | Relation Name [Simple]
                | Implies Name [Name] [PrologTerm]
                deriving (Eq,Show,Read)

type Prolog = [PrologTerm]

data Subst = Subst { sVar :: Name
                   , sValue :: Name
                   } deriving (Eq,Show,Read)


lookupSubst :: Name -> [Subst] -> Maybe Name
lookupSubst name [] = Nothing
lookupSubst name (s : rest)
  | sVar s == name  =  Just $ sValue s
  | otherwise       =  lookupSubst name rest

unify :: [Subst] -> [Simple] -> [Simple] -> Maybe [Subst]
unify ss [] [] = Just ss
unify ss (_:_) [] = Nothing
unify ss [] (_:_) = Nothing
unify ss (x:xs) (y:ys) = case (x,y) of
  (Const a, Const b) | a == b -> unify ss xs ys
                     | otherwise -> Nothing
  (Var x, Const b) -> case lookupSubst x ss of
                        Nothing -> unify (Subst x b : ss) xs ys
                        Just value
                          | value == b -> unify ss xs ys
                          | otherwise -> Nothing
  (Const a, Var y) -> case lookupSubst y ss of
                        Nothing -> unify (Subst y a : ss) xs ys
                        Just value
                          | value == a -> unify ss xs ys
                          | otherwise -> Nothing
  (Any, _) -> unify ss xs ys
  (_, Any) -> unify ss xs ys
  (_, _) -> Nothing

isCompatible :: Name -> [Simple] -> PrologTerm -> Maybe Subst
isCompatible relName relArgs term = case term of
  Sim _ -> Nothing
  Implies _ _ _ -> Nothing
  Relation name args
    | name == relName -> unify [] relArgs args
    | otherwise -> Nothing

findRelation :: Prolog -> Name -> [Simple] -> Maybe [Subst]
findRelation terms name args =
  listToMaybe $ mapMaybe (isCompatible name args) terms

findImplication :: Prolog -> Name -> [Simple] -> Maybe [Subst]
findImplication _ _ _ = error "Implement me!"

eval :: Prolog -> (Name,[Simple]) -> Maybe [Subst]
eval knowledge (relName, relArgs) = let
  mRelation = findRelation knowledge relName relArgs
  mImplies = findImplication knowledge relName relArgs
  in case mRelation of
       Just relSubst -> Just relSubst
       Nothing -> mImplies


test101 :: Prolog
test101 =
  [ Relation "human" [Const "sokrat"]
  , Relation "human" [Const "platon"]
  , Implies "mortal" ["Someone"]
    [ Relation "human" [Var "Someone"]
    ]
  ]


main = return ()

