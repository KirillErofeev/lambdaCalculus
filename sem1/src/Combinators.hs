module Combinators where

import Types

x = "x"
y = "y"
z = "z"
g = "g"
f = "f"
t = "t"
b = "b"
s'= "s"
p = "p"

test = lam "x" $ lam "x" $ lam "x" $  
    (app 
        (app
            (app
            (lam "b" $ lam "f" $ lam "s" ( app (app (sym "b") (sym "f")) (sym "s"))) 
                 (lam "x" $ lam "y" (sym "x"))) 
                      (lam "x" (sym "x"))) 
                          (lam "x" $ lam "y" (sym "y")))

w   = lam "a" $ app (sym "a") (sym "a")

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

true  = lam t $ lam f $ sym t
false = lam t $ lam f $ sym f
iff   = lam b $ lam t $ lam f $ app3 (sym b) (sym t) (sym f)

pair  = lam f $ lam s' $ lam b $ app3 (sym b) (sym f) (sym s')
fst'   = lam p $ app (sym p) true
snd'   = lam p $ app (sym p) false

omega = app (lam x $ app (sym x) (sym x)) (lam x $ app (sym x) (sym x))
fx   = lam f $ app fx' fx'
fx'  = lam x $ app (sym f) (lam y $ app3 (sym x) (sym x) (sym y))