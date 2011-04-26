{
module Language.SMTLIB.Lexer
  ( Token (..)
  , lexSMTLIB
  , alexScanTokens
  , alexAndPred
  , alexPrevCharIs
  , alexPrevCharIsOneOf
  , alexRightContext
  , iUnbox
  , alexInputPrevChar
  ) where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters
$other = [\+\-\/\*\=\%\?\!\.\$\_\~\&\^\<\>\@]
$sym   = [$digit$alpha$other]
$hex   = [a-fA-F$digit]
$bin   = 0-1

tokens :-

  $white+                            ;
  \;.*                               ;
  [$sym # $digit]$sym*               { Symbol  }
  \|[$printable \n # \|]*\|          { Symbol  }
  \:$sym+                            { Keyword }
  $digit+\.$digit+                   { Decimal . read }
  $digit+                            { Numeral . read }
  \(                                 { const LeftParen  }
  \)                                 { const RightParen }
  \"(([$printable \n # \\]|\\.)*)\"  { String . read }
  "#x"$hex+                          { Hex . drop 2 }
  "#b"$bin+                          { Bin . drop 2 }
  .                                  { \ s -> error $ "unexpected character: '" ++ s ++ "'" }

{
data Token
  = Numeral Integer
  | Decimal Double
  | Hex     String
  | Bin     String
  | String  String
  | Symbol  String
  | Keyword String
  | LeftParen
  | RightParen
  deriving (Eq,Show)

lexSMTLIB :: String -> [Token]
lexSMTLIB a = alexScanTokens a
}
