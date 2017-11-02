module Combinators where

import Types

x = "x"
y = "y"
z = "z"
g = "g"
f = "f"

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

s = lam f $ lam g $ lam x $ app  
    (app (sym f) (sym x))
     (app 
          (sym g) (sym x))

k = lam x $ lam y $ sym x

i = lam x $ sym x

sai = lam f $ app 
    (app (sym f) s)
     (lam x $ lam y $ lam z $ sym x)
