data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add l r) = (eval l) + (eval r)

printExpr :: Expr -> String
printExpr Lit x = show x
printExpr Expr l r = show l ++ " + " ++ show r
