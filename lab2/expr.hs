-- Completed by Chonratid Pangdee, chopan-7@student.ltu.se
-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson

import           Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[])   = True
    notfirst p (_,x:xs) = not (p x)

    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)

    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)


    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys

    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys

    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n)       = show n
unparse (Var s)         = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App oper e1)   =  oper ++ "(" ++ unparse e1 ++ ")"

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env

-- Task 1.1
eval (App "sin" e1) env = sin (eval e1 env)
eval (App "cos" e1) env = cos (eval e1 env)
eval (App "log" e1) env = log (eval e1 env)
eval (App "exp" e1) env = exp (eval e1 env)


diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) = Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) = Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)

-- Task 1.2
diff v (App "sin" e1) = Op "*" (diff v e1) (App "cos" e1)
diff v (App "cos" e1) = Op "*" (diff v e1) (Op "-" (Const 0) (App "sin" e1))
diff v (App "log" e1) = Op "*" (Op "/" (Const 1) e1) (diff v e1)
diff v (App "exp" e1) = Op "*" (diff v e1) (App "exp" e1)
diff _ _ = error "can not compute the derivative"

simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re

-- Task 1.3
simplify (App f e1) =
 let expressions = simplify e1 in
  case (f, expressions) of
    ("sin", e) -> App "sin" e
    ("cos", e) -> App "cos" e
    ("log", e) -> App "log" e
    ("exp", e) -> App "exp" e

-- Task 2
mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (body, (Var var)) = \x -> eval body [(var, x)]

-- Task 3
findZero :: String -> String -> Float -> Float
findZero s1 s2 x0 = nwtRaph f fprime x0
 where f = mkfun ((parse s2), (Var s1))
       fprime = mkfun ((diff (Var s1) (parse s2)), (Var s1))
       nwtRaph :: (Float -> Float) -> (Float -> Float) -> Float -> Float
       nwtRaph f f' x0
        | delta > 0 && delta <= 0.0001 = x1
        | otherwise = nwtRaph f f' x1
        where x1 = x0-(f x0)/(f' x0)
              delta = abs (x1-x0)
