data Var = X | Y | Z deriving Eq

data AExp = N Integer | V Var
          | AExp :+ AExp | AExp :- AExp | AExp :* AExp
          | Iff BExp AExp AExp
          | Stmt :==-> AExp

data BExp = TT | FF | AExp :== AExp | AExp :<= AExp 
          | Not BExp | BExp :&& BExp | BExp :|| BExp

data Stmt = Skip | Stmt :\ Stmt
          | Var := AExp
          | If BExp Stmt Stmt
          | While BExp Stmt

type State = [(Var, Integer)]

lkp :: Var -> State -> Integer
lkp x [] = 0
lkp x ((y,z):s) = if x == y then z else lkp x s

upd :: Var -> Integer -> State -> State
upd x z [] = [(x,z)]
upd x z ((y,w):s) = if x == y then (x,z):s
                       else (y,w):upd x z s

init :: State
init = []

show :: State -> String
show = undefined

aexp :: AExp -> State -> Integer
aexp (N z) _      = z
aexp (V x) s      = lkp x s
aexp (a0 :+ a1) s = aexp a0 s + aexp a1 s
aexp (a0 :- a1) s = aexp a0 s - aexp a1 s
aexp (a0 :* a1) s = aexp a0 s * aexp a1 s
aexp (Iff b0 a0 a1) s = if bexp b0 s then
                          aexp a0 s
                        else
                          aexp a1 s
aexp (stm :==-> a) s = aexp a (stmt stm s)

bexp :: BExp -> State -> Bool
bexp TT _ = True
bexp FF _ = False
bexp (a0 :== a1) s = aexp a0 s == aexp a1 s
bexp (a0 :<= a1) s = aexp a0 s <= aexp a1 s
bexp (Not b) s = not (bexp b s)
bexp (a0 :&& a1) s = bexp a0 s && bexp a1 s
bexp (a0 :|| a1) s = bexp a0 s || bexp a1 s

stmt :: Stmt -> State -> State
stmt Skip s = s
stmt (stm0 :\ stm1) s = s' where
                          s'' = stmt stm0 s
                          s'  = stmt stm1 s''
stmt (x := a) s = upd x z s where
                          z = aexp a s
stmt (If b stm0 stm1) s =
    if bexp b s then
      stmt stm0 s
    else stmt stm1 s
stmt (While b stm0) s =
    if bexp b s then 
      stmt (While b stm0) (stmt stm0 s)
    else s


test = aexp (Iff ((N 3) :== (N 3)) (N 4) (N 2)) s where s = Main.init

stateForTest2 = upd X 4 (upd Y 7 Main.init)
test2 = aexp ((X := ((V X) :+ (N 1))) :==-> ((V X) :+ (V Y))) stateForTest2

