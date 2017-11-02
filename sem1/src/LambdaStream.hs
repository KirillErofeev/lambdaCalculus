module LambdaStream 
     --(lambdaStream) 
         where

import Types
import Combinators

lambdaStream = concatMap lambdas [1..]

lambdas 1 = [sai]
lambdas n = concatMap makeLambdas (splits n)

makeLambdas (l,r) = map (uncurry app) (pairs (lambdas l) (lambdas r))  

splits n = [ (x, n-x) | x<-[1..n-1]]

pairs x y = [(a,b) | a<-x, b<-y]
