data Var = X | Y | Z deriving Eq

data AExp = N Integer | V Var
          | AExp :+ AExp | AExp :- AExp | AExp :* AExp

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

bexp :: BExp -> State -> Bool
bexp TT _ = True
bexp FF _ = False
bexp (a0 :== a1) s = aexp a0 s == aexp a1 s
bexp (a0 :<= a1) s = aexp a0 s <= aexp a1 s
bexp (Not b) s = not (bexp b s)
bexp (a0 :&& a1) s = bexp a0 s && bexp a1 s
bexp (a0 :|| a1) s = bexp a0 s || bexp a1 s

type CounterState = (Integer, State)

countOf :: CounterState -> Integer
countOf (n, s) = n
stateOf :: CounterState -> State
stateOf (n, s) = s

stmt :: Stmt -> State -> CounterState
stmt Skip s = (1, s)
stmt (stm0 :\ stm1) s = (n' + n'', s'') where
                          first = stmt stm0 s
                          s'    = stateOf first
                          n'    = countOf first
                          last  = stmt stm1 s'
                          s''   = stateOf last
                          n''   = countOf last
stmt (x := a) s = (1, upd x z s) where
                          z = aexp a s
stmt (If b stm0 stm1) s =
    if bexp b s then
      let result = stmt stm0 s
      in (countOf(result) + 1, stateOf result)
    else 
      let result = stmt stm1 s
      in (countOf(result) + 1, stateOf result)

stmt (While b stm0) s =
    if bexp b s then 
      let result  = stmt stm0 s
          s'      = stateOf result
          result' = stmt (While b stm0) s'
      in (countOf(result) + countOf(result') + 1, stateOf(result'))
    else (1, s)


