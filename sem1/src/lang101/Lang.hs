{-# LANGUAGE TupleSections #-}

module Main where

import Data.Maybe

type Name = String

data Simple = Const Name
            | Nat Int
            | Var Name
            | Arr [Simple]
            | Any
            deriving (Eq,Show,Read)

data PrologTerm = Sim Simple
                | Relation Name [Simple]
                | Function Name [Name] [PrologTerm]
                deriving (Eq,Show,Read)

type Prolog = [PrologTerm]

data Ans = BoolAns Bool
         | VarAns [(Name, Simple)]
         deriving (Eq,Show,Read)

data Fact = Ident Simple
          | Rel Name [Simple]
          deriving (Eq,Show,Read)
type Knowledge = [Fact]

toIdent :: Fact -> Maybe Simple
toIdent (Ident s) = Just s
toIdent _ = Nothing

constructSubst :: [Name] -> [Simple] -> [[(Name,Simple)]]
constructSubst [] _ = []
constructSubst [x] facts = map (pure . (x,)) facts
constructSubst (arg : args) facts = let
  firstSubst = map (arg,) facts
  restSubst = constructSubst args facts
  in [first : rest | first <- firstSubst, rest <- restSubst]

substitute :: [(Name, Simple)] -> Simple -> Simple
substitute subst (Var name) = case lookup name subst of
  Nothing -> Var name
  Just s -> s
substitute _ s = s

applySubst :: Name -> [Simple] -> [(Name,Simple)] -> Fact
applySubst relName relArgs subst =
  Rel relName $ map (substitute subst) relArgs

apply :: Name -> [Name] -> Knowledge -> PrologTerm -> Knowledge
apply n args knowledge (Sim s) = [Ident s]
apply n args knowledge (Relation m ts) = let
  allConsts = mapMaybe toIdent knowledge
  allArgs = constructSubst args allConsts
  in map (applySubst n ts) allArgs

updateKnowledgeBase :: Knowledge
                    -> PrologTerm
                    -> Knowledge
updateKnowledgeBase ks (Sim s) = Ident s : ks
updateKnowledgeBase ks (Relation n ss) = Rel n ss : ks
updateKnowledgeBase ks (Function n args terms) =
  concatMap (apply n args ks) terms ++ ks

eval :: Prolog -> (Name, [Simple]) -> Ans
eval terms (name, args) = error "Implement me!"


test101 :: Prolog
test101 =
  [ Relation "human" [Const "sokrat"]
  , Relation "human" [Const "platon"]
  , Function "mortal" ["Someone"]
    [ Relation "human" [Var "Someone"]
    ]
  ]

test :: Prolog
test =
  [ Function "neighbors" ["X", "Y", "List"]
    [ Relation "nextto" [Var "X", Var "Y", Var "List"]
    , Relation "nextto" [Var "Y", Var "X", Var "List"]
    ]
    -- 1. Норвежец живёт в первом доме.
  , Relation "nth" [ Nat 1
                   , Var "Houses"
                   , Arr [ Const "norwegian"
                         , Any
                         , Any
                         , Any
                         , Any
                         ]
                   ]
    -- 2. Англичанин живёт в красном доме.
  , Relation "member" [ Arr [ Const "englishman"
                            , Any
                            , Any
                            , Any
                            , Const "red"
                            ]
                      , Var "Houses"
                      ]
    -- 3. Зелёный дом находится слева от белого, рядом с ним.
  , Relation "nextto" [ Arr [ Any
                            , Any
                            , Any
                            , Any
                            , Const "green"
                            ]
                      , Arr [ Any
                            , Any
                            , Any
                            , Any
                            , Const "white"]
                      , Var "Houses"
                      ]
  ]

main = return ()

