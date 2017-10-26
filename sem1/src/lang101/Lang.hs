{-# LANGUAGE TupleSections #-}

module Main where

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

type Knowledge = [PrologTerm]

toIdent :: Fact -> Maybe Simple
toIdent (Ident s) = Just s
toIdent _ = Nothing


updateKB :: Knowledge -> PrologTerm -> Knowledge
updateKB = error "Implement me!"


test101 :: Prolog
test101 =
  [ Relation "human" [Const "sokrat"]
  , Relation "human" [Const "platon"]
  , Implies "mortal" ["Someone"]
    [ Relation "human" [Var "Someone"]
    ]
  ]


main = return ()

