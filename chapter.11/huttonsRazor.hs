module HuttonsRazor where
  data Expr = Lit Integer | Add Expr Expr

  eval :: Expr -> Integer
  eval (Lit x) = x
  eval (Add expr expr') = (eval expr) + (eval expr')

  printExpr :: Expr -> String
  printExpr (Lit x) = show x
  printExpr (Add expr expr') = (printExpr expr) ++ " + " ++ (printExpr expr')
