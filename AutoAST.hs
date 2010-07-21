module AutoAST
  ( version12
  ) where

version12 :: String
version12 = unlines
  [ "module Language.SMTLIB.Version12 () where"
  ]

data Term
  = NonTerminal String [(String, Expr)]
  | Terminal String

data Expr
  = One      Term
  | Many     Term
  | Sequence [Expr]
  | Token    String

many1 :: Term -> Expr
many1 a = Sequence [One a, Many a]

left :: Expr
left = Token "LeftParan"

right :: Expr
right = Token "RightParan"

ident :: String -> Expr
ident a = Token $ "Identifier \"" ++ a ++ "\""

fun_symb = NonTerminal "fun_symb"
  [ ("fun_symb_identifier", One identifier)
  , ("fun_symb_arith_symb", One arith_symb)
  ]

an_term = NonTerminal "an_term"
  [ ("an_term_base_term", One base_term)
  , ("an_term_base_term_", Sequence [left, One base_term, Many annotation, right])
  , ("an_term_fun", Sequence [left, One fun_symb, many1 an_term, Many annotation, right])
  , ("an_term_ite", Sequence [left, ident "ite", One an_formula, One an_term, Many annotation, right])
  ]

annotation = undefined
an_formula = undefined
base_term = undefined
arith_symb = undefined
identifier = undefined
