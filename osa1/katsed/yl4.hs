data Var = X | Y | Z deriving Eq

data AExp = N Integer | V Var
          | AExp :+ AExp | AExp :- AExp | AExp :* AExp
          | Var ::= AExp

data BExp = TT | FF | AExp :== AExp | AExp :<= AExp 
          | Not BExp | BExp :&& BExp | BExp :|| BExp

data Stmt = Skip | Stmt :\ Stmt
          | Var := AExp
          | If BExp Stmt Stmt
          | While BExp Stmt

type State = [(Var, Integer)]

{-Aritmeetikaavaldiste tulemuste hoidmine-}
type AResult = (Integer, State)

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

stateOf :: AResult -> State
stateOf (z, s) = s
zOf :: AResult -> Integer
zOf (z, s) = z

aexp :: AExp -> State -> AResult
aexp (N z) s      = (z, s)
aexp (V x) s      = (lkp x s, s)
aexp (x ::= a) s   = (z, upd x z s') where
                          result = aexp a s
                          z = zOf result
                          s' = stateOf result
aexp (a0 :+ a1) s = let first = aexp a0 s
                        last = aexp a1 (stateOf first)
                    in (zOf(first) + zOf(last), stateOf(last))
aexp (a0 :- a1) s = let first = aexp a0 s
                        last = aexp a1 (stateOf first)
                    in (zOf(first) - zOf(last), stateOf(last))
aexp (a0 :* a1) s = let first = aexp a0 s
                        last = aexp a1 (stateOf first)
                    in (zOf(first) * zOf(last), stateOf(last))

type BResult = (Bool, State)
stateOfB :: BResult -> State
stateOfB (z, s) = s
bOf :: BResult -> Bool
bOf (b, s) = b

bexp :: BExp -> State -> BResult
bexp TT s = (True, s)
bexp FF s = (False, s)
bexp (a0 :== a1) s = let first = aexp a0 s
                         last = aexp a1 (stateOf first)
                     in (zOf(first) == zOf(last), stateOf(last))
bexp (a0 :<= a1) s = let first = aexp a0 s
                         last = aexp a1 (stateOf first)
                     in (zOf(first) == zOf(last), stateOf(last))
bexp (b0 :&& b1) s = let first = bexp b0 s
                         last = bexp b1 (stateOfB first)
                     in (bOf(first) && bOf(last), stateOfB(last))
bexp (b0 :|| b1) s = let first = bexp b0 s
                         last = bexp b1 (stateOfB first)
                     in (bOf(first) || bOf(last), stateOfB(last))
bexp (Not b) s = let result = bexp b s
                 in (not (bOf result), stateOfB result)

stmt :: Stmt -> State -> State
stmt Skip s = s
stmt (stm0 :\ stm1) s = s' where
                          s'' = stmt stm0 s
                          s'  = stmt stm1 s''
stmt (x := a) s = upd x z s' where
                          result = aexp a s
                          z = zOf result
                          s' = stateOf result
stmt (If b stm0 stm1) s =
    let result = bexp b s
    in if bOf result then
         stmt stm0 (stateOfB(result))
       else stmt stm1 (stateOfB(result))

stmt (While b stm0) s =
    let result = bexp b s
    in if bOf result then 
         stmt (While b stm0) (stmt stm0 (stateOfB(result)))
       else s

test = aexp((X ::= ((N 1) :+ (V X))) :+ (V X)) []
