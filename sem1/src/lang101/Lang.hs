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



unify :: Simple -> Simple -> Maybe Subst
unify (Const a) (Const b) = a == b
unify (Var x) (Const b) = True
unify (Const a) (Var y) = True
unify Any _ = True
unify _ Any = True
unify _ _ = False

isCompatible :: Name -> [Simple] -> PrologTerm -> Maybe Subst
isCompatible relName relArgs term = case term of
  Sim _ -> Nothing
  Implies _ _ _ -> Nothing
  Relation name args |
    name == relName &&
    length relArgs == length args ->
      forM_ (zip relArgs args) $ \(a,b) -> do
        unify relArgs args

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

