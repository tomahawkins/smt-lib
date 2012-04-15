-- | Parsing and printing SMT-LIB.

{-# language OverloadedStrings #-}

module Language.SMTLIB
  (
  -- * Syntax
    Numeral
  , Symbol
  , Keyword
  , Spec_constant        (..)
  , S_expr               (..)
  , Identifier           (..)
  , Sort                 (..)
  , Attribute_value      (..)
  , Attribute            (..)
  , Qual_identifier      (..)
  , Var_binding          (..)
  , Sorted_var           (..)
  , Term                 (..)
  , Sort_symbol_decl     (..)
  , Meta_spec_constant   (..)
  , Fun_symbol_decl      (..)
  , Par_fun_symbol_decl  (..)
  , Theory_attribute     (..)
  , Theory_decl          (..)
  , Logic_attribute      (..)
  , Logic                (..)
  , Option               (..)
  , Info_flag            (..)
  , Command              (..)
  , Script               (..)
  , Gen_response         (..)
  , Error_behavior       (..)
  , Reason_unknown       (..)
  , Status               (..)
  , Info_response        (..)
  , Proof
  , Valuation_pair
  , T_valuation_pair
  , Command_response     (..)
  -- * Parsing
  , parseScript
  , parseResponses
  , parseTheory
  , parseLogic
  -- * Parsing Verification
  , checkScript
  , checkResponses
  , checkParser
  ) where

import Data.List hiding (group, (++))
import System.Directory
import System.IO
import Text.ParserCombinators.Poly.Lazy hiding (Success)
import Text.Printf

import Prelude hiding ((++))
import qualified Prelude
import Data.Monoid (mappend, Monoid)
import Text.PrettyPrint.HughesPJ
import Data.Char ( toLower )

import Language.SMTLIB.Lexer

class Pretty a where pretty :: a -> Doc

pgroup :: [ Doc ] -> Doc
pgroup xs = parens $ fsep xs

(++) :: Monoid a => a -> a -> a
(++) = mappend

type Numeral      = Integer
type Symbol       = String
type Keyword      = String

instance Pretty Integer where pretty = text . show
instance Pretty Int where pretty = text . show
instance Pretty Bool where 
  pretty = text . map toLower . show

data Spec_constant
  = Spec_constant_numeral     Numeral
  | Spec_constant_decimal     Rational
  | Spec_constant_hexadecimal String
  | Spec_constant_binary      [Bool]
  | Spec_constant_string      String

instance Pretty Spec_constant where
  pretty a = case a of
    Spec_constant_numeral     a -> pretty a
    Spec_constant_decimal     a -> text $ show (realToFrac a :: Double)
    Spec_constant_hexadecimal a -> text $ printf "#x%s" a
    Spec_constant_binary      a -> text $ printf "#b%s" [ if a then '1' else '0' | a <- a ]
    Spec_constant_string      a -> text a


spec_constant :: SMTLIB Spec_constant
spec_constant = oneOf
  [ numeral >>= return . Spec_constant_numeral
  , string  >>= return . Spec_constant_string
  , do
      a <- satisfy (\ a -> case a of { Decimal _ -> True; Hex _ -> True; Bin _ -> True; _ -> False })
      case a of
        Decimal a -> return $ Spec_constant_decimal $ toRational a
        Hex     a -> return $ Spec_constant_hexadecimal a
        Bin     a -> return $ Spec_constant_binary $ map (== '1') a
        _ -> undefined
  ]

data S_expr
  = S_expr_constant Spec_constant
  | S_expr_symbol   Symbol
  | S_expr_keyword  Keyword
  | S_exprs         [S_expr]

instance Pretty S_expr where
  pretty a = case a of
    S_expr_constant a -> pretty a
    S_expr_symbol   a -> text a
    S_expr_keyword  a -> text a
    S_exprs         a -> group $ map pretty a

s_expr :: SMTLIB S_expr
s_expr = oneOf
  [ spec_constant >>= return . S_expr_constant
  , symbol        >>= return . S_expr_symbol
  , keyword       >>= return . S_expr_keyword
  , do { left; a <- many s_expr; right; return $ S_exprs a }
  ]

data Identifier
  = Identifier  Symbol
  | Identifier_ Symbol [Numeral]

instance Pretty Identifier where
  pretty a = case a of
    Identifier  a -> text a
    Identifier_ a b -> group $ ["_", text a] ++ map pretty b


identifier :: SMTLIB Identifier
identifier = oneOf
  [ symbol >>= return . Identifier
  , do { left; tok (Symbol "_"); a <- symbol; b <- many1 numeral; right; return $ Identifier_ a b }
  ]

data Sort
  = Sort_bool
  | Sort_identifier  Identifier
  | Sort_identifiers Identifier [Sort]

instance Pretty Sort where 
  pretty a = case a of
    Sort_bool -> text "Bool"
    Sort_identifier  a -> pretty a
    Sort_identifiers a b -> pgroup $ pretty a : map pretty b 

sort' :: SMTLIB Sort
sort' = oneOf
  [ tok (Symbol "Bool") >> return Sort_bool
  , identifier >>= return . Sort_identifier
  , do { left; a <- identifier; b <- many1 sort'; right; return $ Sort_identifiers a b }
  ]

data Attribute_value
  = Attribute_value_spec_constant Spec_constant
  | Attribute_value_symbol        Symbol
  | Attribute_value_s_expr        [S_expr]

instance Pretty Attribute_value where
  pretty a = case a of
    Attribute_value_spec_constant a -> pretty a
    Attribute_value_symbol        a -> text a
    Attribute_value_s_expr        a -> group $ map pretty a

attribute_value :: SMTLIB Attribute_value
attribute_value = oneOf
  [ spec_constant >>= return . Attribute_value_spec_constant
  , symbol        >>= return . Attribute_value_symbol
  , do { left; a <- many s_expr; right; return $ Attribute_value_s_expr a }
  ]

data Attribute
  = Attribute        Keyword
  | Attribute_s_expr Keyword S_expr

instance Pretty Attribute where
  pretty a = case a of
    Attribute        a -> text a
    Attribute_s_expr a b -> text a ++ " " ++ pretty b

attribute :: SMTLIB Attribute
attribute = oneOf
  [ do { a <- keyword; b <- s_expr; return $ Attribute_s_expr a b }
  , keyword >>= return . Attribute
  ]

data Qual_identifier
  = Qual_identifier      Identifier
  | Qual_identifier_sort Identifier Sort

instance Pretty Qual_identifier where
  pretty a = case a of
    Qual_identifier      a -> pretty a
    Qual_identifier_sort a b -> group ["as", pretty a, pretty b]

                                
qual_identifier :: SMTLIB Qual_identifier
qual_identifier = oneOf
  [ identifier >>= return . Qual_identifier
  , do { left; tok $ Symbol "as"; a <- identifier; b <- sort'; right; return $ Qual_identifier_sort a b }
  ]

data Var_binding
  = Var_binding Symbol Term


instance Pretty Var_binding where
  pretty a = case a of
    Var_binding a b -> pgroup [ text a, pretty b ]

var_binding :: SMTLIB Var_binding
var_binding = do { left; a <- symbol; b <- term; right; return $ Var_binding a b }

data Sorted_var
  = Sorted_var Symbol Sort

instance Pretty Sorted_var where
  pretty a = case a of
    Sorted_var a b -> group [text a, pretty b]


sorted_var :: SMTLIB Sorted_var
sorted_var = do { left; a <- symbol; b <- sort'; right; return $ Sorted_var a b }

data Term
  = Term_spec_constant    Spec_constant
  | Term_qual_identifier  Qual_identifier
  | Term_qual_identifier_ Qual_identifier [Term]
  | Term_distinct         Term [Term]
  | Term_let              [Var_binding] Term
  | Term_forall           [Sorted_var] Term
  | Term_exists           [Sorted_var] Term
  | Term_attributes       Term [Attribute]

instance Pretty Term where
  pretty a = case a of
    Term_spec_constant    a -> pretty a
    Term_qual_identifier  a -> pretty a
    Term_qual_identifier_ a b -> group $ pretty a : map pretty b
    Term_distinct         a b -> group $ ["distinct", pretty a] ++ map pretty b
    Term_let              a b -> group $ ["let",    group $ map pretty a, pretty b]
    Term_forall           a b -> group $ ["forall", group $ map pretty a, pretty b]
    Term_exists           a b -> group $ ["exists", group $ map pretty a, pretty b]
    Term_attributes       a b -> group $ ["!", pretty a] ++ map pretty b




term :: SMTLIB Term
term = oneOf
  [ spec_constant   >>= return . Term_spec_constant
  , qual_identifier >>= return . Term_qual_identifier
  , do { left; a <- qual_identifier; b <- many1 term; right; return $ Term_qual_identifier_ a b }
  , do { left; tok $ Symbol "distinct"; a <- term; b <- many1 term; right; return $ Term_distinct a b }
  , do { left; tok $ Symbol "let";    left; a <- many1 var_binding; right; b <- term; right; return $ Term_let a b }
  , do { left; tok $ Symbol "forall"; left; a <- many1 sorted_var;  right; b <- term; right; return $ Term_forall a b }
  , do { left; tok $ Symbol "exists"; left; a <- many1 sorted_var;  right; b <- term; right; return $ Term_exists a b }
  , do { left; tok $ Symbol "!"; a <- term;  b <- many1 attribute; right; return $ Term_attributes a b }
  ]

data Sort_symbol_decl
  = Sort_symbol_decl Identifier Numeral [Attribute]

instance Pretty Sort_symbol_decl where
  pretty a = case a of
    Sort_symbol_decl a b c -> group $ [pretty a, pretty b] ++ map pretty c

sort_symbol_decl :: SMTLIB Sort_symbol_decl
sort_symbol_decl = do { left; a <- identifier; b <- numeral; c <- many attribute; right; return $ Sort_symbol_decl a b c }

data Meta_spec_constant
  = Meta_spec_constant_numeral
  | Meta_spec_constant_decimal
  | Meta_spec_constant_string

instance Pretty Meta_spec_constant where
  pretty a = case a of
    Meta_spec_constant_numeral -> "NUMERAL"
    Meta_spec_constant_decimal -> "DECIMAL"
    Meta_spec_constant_string  -> "STRING"

meta_spec_constant :: SMTLIB Meta_spec_constant
meta_spec_constant = oneOf
  [ do { tok $ Symbol "NUMERAL"; return Meta_spec_constant_numeral }
  , do { tok $ Symbol "DECIMAL"; return Meta_spec_constant_decimal }
  , do { tok $ Symbol "STRING" ; return Meta_spec_constant_string  }
  ]

data Fun_symbol_decl
  = Fun_symbol_decl_spec_constant      Spec_constant      Sort [Attribute]
  | Fun_symbol_decl_meta_spec_constant Meta_spec_constant Sort [Attribute]
  | Fun_symbol_decl                    Identifier [Sort] [Attribute]

instance Pretty Fun_symbol_decl where
  pretty a = case a of
    Fun_symbol_decl_spec_constant      a b c -> group $ [pretty a, pretty b] ++ map pretty c
    Fun_symbol_decl_meta_spec_constant a b c -> group $ [pretty a, pretty b] ++ map pretty c
    Fun_symbol_decl                    a b c -> group $ [pretty a] ++ map pretty b ++ map pretty c

fun_symbol_decl :: SMTLIB Fun_symbol_decl
fun_symbol_decl = oneOf
  [ do { left; a <- spec_constant;      b <- sort'; c <- many attribute; right; return $ Fun_symbol_decl_spec_constant      a b c }
  , do { left; a <- meta_spec_constant; b <- sort'; c <- many attribute; right; return $ Fun_symbol_decl_meta_spec_constant a b c }
  , do { left; a <- identifier; b <- many1 sort'; c <- many attribute; right; return $ Fun_symbol_decl a b c }
  ]

data Par_fun_symbol_decl
  = Par_fun_symbol_decl Fun_symbol_decl
  | Par_fun_symbol_decl_symbols [Symbol] Identifier [Sort] [Attribute]

instance Pretty Par_fun_symbol_decl where
  pretty a = case a of
    Par_fun_symbol_decl a -> pretty a
    Par_fun_symbol_decl_symbols a b c d -> 
      group ["par", group $ map text a
            , group $ [pretty b] ++ map pretty c ++ map pretty d]

par_fun_symbol_decl :: SMTLIB Par_fun_symbol_decl
par_fun_symbol_decl = oneOf
  [ fun_symbol_decl >>= return . Par_fun_symbol_decl
  , do { left; tok $ Symbol "par"; left; a <- many1 symbol; right; left; b <- identifier; c <- many1 sort'; d <- many attribute; right; right; return $ Par_fun_symbol_decl_symbols a b c d }
  ]

data Theory_attribute
  = Theory_attribute_sorts [Sort_symbol_decl]
  | Theory_attribute_funs  [Par_fun_symbol_decl]
  | Theory_attribute_sorts_desc String
  | Theory_attribute_funs_desc  String
  | Theory_attribute_definition String
  | Theory_attribute_values     String
  | Theory_attribute_notes      String
  | Theory_attribute            Attribute

instance Pretty Theory_attribute where
  pretty a = case a of
    Theory_attribute_sorts      a -> ":sorts " ++ group (map pretty a)
    Theory_attribute_funs       a -> ":funs "  ++ group (map pretty a)
    Theory_attribute_sorts_desc a -> ":sorts-description " ++ text a
    Theory_attribute_funs_desc  a -> ":funs-description "  ++ text a
    Theory_attribute_definition a -> ":definition "        ++ text a
    Theory_attribute_values     a -> ":values "            ++ text a
    Theory_attribute_notes      a -> ":notes "             ++ text a
    Theory_attribute            a -> pretty a

theory_attribute :: SMTLIB Theory_attribute
theory_attribute = oneOf
  [ do { tok $ Keyword ":sorts"; left; a <- many1 sort_symbol_decl; right; return $ Theory_attribute_sorts a }
  , do { tok $ Keyword ":funs";  left; a <- many1 par_fun_symbol_decl; right; return $ Theory_attribute_funs a }
  , do { tok $ Keyword ":sorts-description"; a <- string; return $ Theory_attribute_sorts_desc a }
  , do { tok $ Keyword ":funs-description"; a <- string; return $ Theory_attribute_funs_desc a }
  , do { tok $ Keyword ":definition"; a <- string; return $ Theory_attribute_definition a }
  , do { tok $ Keyword ":values"; a <- string; return $ Theory_attribute_values a }
  , do { tok $ Keyword ":notes"; a <- string; return $ Theory_attribute_notes a }
  , attribute >>= return . Theory_attribute
  ]

data Theory_decl
  = Theory_decl Symbol [Theory_attribute]

instance Pretty Theory_decl where
  pretty a = case a of
    Theory_decl a b -> group $ ["theory", text a] ++ map pretty b

theory_decl :: SMTLIB Theory_decl
theory_decl = do { left; tok $ Symbol "theory"; a <- symbol; b <- many1 theory_attribute; right; return $ Theory_decl a b }

data Logic_attribute
  = Logic_attribute_theories   [Symbol]
  | Logic_attribute_language   String
  | Logic_attribute_extensions String
  | Logic_attribute_values     String
  | Logic_attribute_notes      String
  | Logic_attribute            Attribute

instance Pretty Logic_attribute where
  pretty a = case a of
    Logic_attribute_theories    a -> ":theories " ++ group (map text a)
    Logic_attribute_language    a -> ":language "   ++ text a
    Logic_attribute_extensions  a -> ":extensions " ++ text a
    Logic_attribute_values      a -> ":values "     ++ text a
    Logic_attribute_notes       a -> ":notes "      ++ text a
    Logic_attribute             a -> pretty a

logic_attribute :: SMTLIB Logic_attribute
logic_attribute = oneOf
  [ do { tok $ Keyword ":theories"; left; a <- many1 symbol; right; return $ Logic_attribute_theories a }
  , do { tok $ Keyword ":language"; left; a <- string; right; return $ Logic_attribute_language a }
  , do { tok $ Keyword ":extensions"; left; a <- string; right; return $ Logic_attribute_extensions a }
  , do { tok $ Keyword ":values"; left; a <- string; right; return $ Logic_attribute_values a }
  , do { tok $ Keyword ":notes"; left; a <- string; right; return $ Logic_attribute_notes a }
  , attribute >>= return . Logic_attribute
  ]

data Logic
  = Logic Symbol [Logic_attribute]

instance Pretty Logic where
  pretty a = case a of
    Logic a b -> group $ ["logic", text a] ++ map pretty b

logic :: SMTLIB Logic
logic = do { left; tok $ Symbol "logic"; a <- symbol; b <- many1 logic_attribute; right; return $ Logic a b }

data Option
  = Print_success       Bool
  | Expand_definitions  Bool
  | Interactive_mode    Bool
  | Produce_proofs      Bool
  | Produce_unsat_cores Bool
  | Produce_models      Bool
  | Produce_assignments Bool
  | Regular_output_channel String
  | Diagnostic_output_channel String
  | Random_seed Int
  | Verbosity Int
  | Option_attribute Attribute

instance Pretty Option where
  pretty a = case a of
    Print_success             a -> ":print-success "             ++ pretty a
    Expand_definitions        a -> ":expand-definitions "        ++ pretty a
    Interactive_mode          a -> ":interactive-mode "          ++ pretty a
    Produce_proofs            a -> ":produce-proofs "            ++ pretty a
    Produce_unsat_cores       a -> ":produce-unsat-cores "       ++ pretty a
    Produce_models            a -> ":produce-models "            ++ pretty a
    Produce_assignments       a -> ":produce-assignments "       ++ pretty a
    Regular_output_channel    a -> ":regular-output-channel "    ++ text a
    Diagnostic_output_channel a -> ":diagnostic-output-channel " ++ text a
    Random_seed               a -> ":random-seed "               ++ pretty a
    Verbosity                 a -> ":verbosity "                 ++ pretty a
    Option_attribute          a -> pretty a

option :: SMTLIB Option
option = oneOf
  [ do { tok $ Symbol ":print-success";       a <- b_value; return $ Print_success       a }
  , do { tok $ Symbol ":expand-definitions";  a <- b_value; return $ Expand_definitions  a }
  , do { tok $ Symbol ":interactive-mode";    a <- b_value; return $ Interactive_mode    a }
  , do { tok $ Symbol ":produce-proofs";      a <- b_value; return $ Produce_proofs      a }
  , do { tok $ Symbol ":produce-unsat-cores"; a <- b_value; return $ Produce_unsat_cores a }
  , do { tok $ Symbol ":produce-models";      a <- b_value; return $ Produce_models      a }
  , do { tok $ Symbol ":produce-assignments"; a <- b_value; return $ Produce_assignments a }
  , do { tok $ Symbol ":regular-output-channel";    a <- string; return $ Regular_output_channel    a }
  , do { tok $ Symbol ":diagnostic-output-channel"; a <- string; return $ Diagnostic_output_channel a }
  , do { tok $ Symbol ":random-seed"; a <- numeral; return $ Random_seed $ fromIntegral a }
  , do { tok $ Symbol ":verbosity";   a <- numeral; return $ Verbosity   $ fromIntegral a }
  , attribute >>= return . Option_attribute
  ]

data Info_flag
  = Error_behavior
  | Name
  | Authors
  | Version
  | Status
  | Reason_unknown
  | Info_flag Keyword
  | All_statistics

instance Pretty Info_flag where
  pretty a = case a of
    Error_behavior -> ":error-behavior"
    Name           -> ":name"
    Authors        -> ":authors"
    Version        -> ":version"
    Status         -> ":status"
    Reason_unknown -> ":reason-unknown"
    Info_flag    a -> text a
    All_statistics -> ":all-statistics"

info_flag :: SMTLIB Info_flag
info_flag = oneOf
  [ do { tok $ Keyword ":error-behavior"; return Error_behavior }
  , do { tok $ Keyword ":name"          ; return Name           }
  , do { tok $ Keyword ":authors"       ; return Authors        }
  , do { tok $ Keyword ":version"       ; return Version        }
  , do { tok $ Keyword ":status"        ; return Status         }
  , do { tok $ Keyword ":reason-unknown"; return Reason_unknown }
  , do { tok $ Keyword ":all-statistics"; return All_statistics }
  , keyword >>= return . Info_flag
  ]

data Command
  = Set_logic Symbol
  | Set_option Option
  | Set_info Attribute
  | Declare_sort Symbol Numeral
  | Define_sort  Symbol [Symbol] Sort
  | Declare_fun  Symbol [Sort] Sort
  | Define_fun   Symbol [Sorted_var] Sort Term
  | Push Int
  | Pop  Int
  | Assert Term
  | Check_sat
  | Get_assertions
  | Get_proof
  | Get_unsat_core
  | Get_value [Term]
  | Get_assignment
  | Get_option Keyword
  | Get_info Info_flag
  | Exit

instance Pretty Command where
  pretty a = case a of
    Set_logic    a -> group ["set-logic", text a]
    Set_option   a -> group ["set-option", pretty a]
    Set_info     a -> group ["set-info", pretty a]
    Declare_sort a b -> group ["declare-sort", text a, pretty b]
    Define_sort  a b c -> group ["define-sort", text a, group (map text b), pretty c]
    Declare_fun  a b c -> group ["declare-fun", text a, group (map pretty b), pretty c]
    Define_fun   a b c d -> group ["define-fun", text a, group (map pretty b), pretty c, pretty d]
    Push a -> group ["push", pretty a]
    Pop  a -> group ["pop",  pretty a]
    Assert a -> group ["assert", pretty a]
    Check_sat -> group ["check-sat"]
    Get_assertions -> group ["get-assertions"]
    Get_proof      -> group ["get-proof"]
    Get_unsat_core -> group ["get-unsat-core"]
    Get_value a -> group ["get-value", group $ map pretty a]
    Get_assignment -> group ["get-assignment"]
    Get_option a -> group ["get-option", text a]
    Get_info   a -> group ["get-info", pretty a]
    Exit -> group ["exit"]

command :: SMTLIB Command
command = oneOf
  [ do { left; tok $ Symbol "set-logic"; a <- symbol; right; return $ Set_logic a }
  , do { left; tok $ Symbol "set-option"; a <- option; right; return $ Set_option a }
  , do { left; tok $ Symbol "set-info"; a <- attribute; right; return $ Set_info a }
  , do { left; tok $ Symbol "declare-sort"; a <- symbol; b <- numeral; right; return $ Declare_sort a b }
  , do { left; tok $ Symbol "define-sort"; a <- symbol; left; b <- many symbol; right; c <- sort'; right; return $ Define_sort a b c }
  , do { left; tok $ Symbol "declare-fun"; a <- symbol; left; b <- many sort'; right; c <- sort'; right; return $ Declare_fun a b c }
  , do { left; tok $ Symbol "define-fun"; a <- symbol; left; b <- many sorted_var; right; c <- sort'; d <- term; right; return $ Define_fun a b c d }
  , do { left; tok $ Symbol "push"; a <- numeral; right; return $ Push $ fromIntegral a }
  , do { left; tok $ Symbol "pop"; a <- numeral; right; return $ Pop $ fromIntegral a }
  , do { left; tok $ Symbol "assert"; a <- term; right; return $ Assert a }
  , do { left; tok $ Symbol "check-sat"; right; return $ Check_sat }
  , do { left; tok $ Symbol "get-assertions"; right; return $ Get_assertions }
  , do { left; tok $ Symbol "get-proof"; right; return $ Get_proof }
  , do { left; tok $ Symbol "get-unsat-core"; right; return $ Get_unsat_core }
  , do { left; tok $ Symbol "get-value"; left; a <- many1 term; right; right; return $ Get_value a }
  , do { left; tok $ Symbol "get-assignment"; right; return $ Get_assignment }
  , do { left; tok $ Symbol "get-option"; a <- keyword; right; return $ Get_option a }
  , do { left; tok $ Symbol "get-info"; a <- info_flag; right; return $ Get_info a }
  , do { left; tok $ Symbol "exit"; right; return $ Exit }
  ]

data Script = Script [Command]

instance Pretty Script where
  pretty (Script a) = vcat $ map pretty a
  
instance Show Script where show = render . pretty  

script :: SMTLIB Script
script = return Script `apply` many command `discard` eof

data Gen_response
  = Unsupported
  | Success
  | Error String

instance Pretty Gen_response where
  pretty a = case a of
    Unsupported  -> "unsupported"
    Success      -> "sucess"
    Error a      -> group ["error", text a]

gen_response :: SMTLIB Gen_response
gen_response = oneOf
  [ do { tok $ Symbol "unsupported"; return Unsupported }
  , do { tok $ Symbol "success"; return Success }
  , do { left; tok $ Symbol "error"; a <- string; right; return $ Error a }
  ]

data Error_behavior
  = Immediate_exit
  | Continued_execution

instance Pretty Error_behavior where
  pretty a = case a of
    Immediate_exit      -> "immediate-exit"
    Continued_execution -> "continued-execution"

error_behavior :: SMTLIB Error_behavior
error_behavior = oneOf
  [ do { tok $ Symbol "immediate-exit"; return Immediate_exit }
  , do { tok $ Symbol "continued-execution"; return Continued_execution }
  ]

data Reason_unknown
  = Timeout
  | Memout
  | Incomplete

instance Pretty Reason_unknown where
  pretty a = case a of
    Timeout    -> "timeout"
    Memout     -> "memout"
    Incomplete -> "incomplete"

reason_unknown :: SMTLIB Reason_unknown
reason_unknown = oneOf
  [ do { tok $ Symbol "timeout"; return Timeout }
  , do { tok $ Symbol "memout"; return Memout }
  , do { tok $ Symbol "incomplete"; return Incomplete }
  ]

data Status
  = Sat
  | Unsat
  | Unknown

instance Pretty Status where
  pretty a = case a of
    Sat     -> "sat"
    Unsat   -> "unsat"
    Unknown -> "unknown"

status :: SMTLIB Status
status = oneOf
  [ do { tok $ Symbol "sat"; return Sat }
  , do { tok $ Symbol "unsat"; return Unsat }
  , do { tok $ Symbol "unknown"; return Unknown }
  ]

data Info_response
  = Info_response_error_behavior Error_behavior
  | Info_response_name    String
  | Info_response_authors String
  | Info_response_version String
  | Info_response_status  Status
  | Info_response_reason_unknown Reason_unknown
  | Info_response_attribute Attribute

instance Pretty Info_response where
  pretty a = case a of
    Info_response_error_behavior a -> ":error-behavior " ++ pretty a
    Info_response_name           a -> ":name "           ++ text a
    Info_response_authors        a -> ":authors "        ++ text a
    Info_response_version        a -> ":version "        ++ text a
    Info_response_status         a -> ":status "         ++ pretty a
    Info_response_reason_unknown a -> ":reason-unknown " ++ pretty a
    Info_response_attribute      a -> pretty a


info_response :: SMTLIB Info_response
info_response = oneOf
  [ do { tok $ Keyword ":error-behavior"; a <- error_behavior; return $ Info_response_error_behavior a }
  , do { tok $ Keyword ":name"; a <- string; return $ Info_response_name a }
  , do { tok $ Keyword ":authors"; a <- string; return $ Info_response_authors a }
  , do { tok $ Keyword ":version"; a <- string; return $ Info_response_version a }
  , do { tok $ Keyword ":status"; a <- status; return $ Info_response_status a }
  , do { tok $ Keyword ":reason-unknown"; a <- reason_unknown; return $ Info_response_reason_unknown a }
  , do attribute >>= return . Info_response_attribute
  ]

type Proof            = S_expr
type Valuation_pair   = (Term, Term)
type T_valuation_pair = (Symbol, Bool)

data Command_response
  = Gen_response  Gen_response
  | Info_response Info_response
  | Gi_response   [Info_response]
  | Cs_response   Status
  | Ga_response   [Term]
  | Gp_response   Proof
  | Guc_response  [Symbol]
  | Gv_response   [Valuation_pair]
  | Gta_response  [T_valuation_pair]

instance Show Command_response where 
   show = render . pretty

instance Pretty Command_response where
  pretty a = case a of
    Gen_response  a -> pretty a
    Info_response a -> pretty a
    Gi_response   a -> group $ map pretty a
    Cs_response   a -> pretty a
    Ga_response   a -> group $ map pretty a
    Gp_response   a -> pretty a
    Guc_response  a -> group $ map text a
    Gv_response   a -> group $ [ group [(pretty a), (pretty b)] | (a, b) <- a ]
    Gta_response  a -> group $ [ group [(text a), (pretty b)] | (a, b) <- a ]

command_response :: SMTLIB Command_response
command_response = oneOf
  [ gen_response >>= return . Gen_response
  , info_response >>= return . Info_response
  , do { left; a <- many1 info_response; right; return $ Gi_response a }
  , status >>= return . Cs_response
  , do { left; a <- many term; right; return $ Ga_response a }
  , s_expr >>= return . Gp_response
  , do { left; a <- many symbol; right; return $ Guc_response a }
  , do { left; a <- many1 (do { left; a <- term; b <- term; return (a, b) }); right; return $ Gv_response a }
  , do { left; a <- many (do { left; a <- symbol; b <- b_value; return (a, b) }); right; return $ Gta_response a }
  ]

responses :: SMTLIB [Command_response]
responses = return id `apply` many command_response `discard` eof

group :: [Doc] -> Doc
group a = 
    -- "( " ++ intercalate " " a ++ " )"
    parens $ fsep a

type SMTLIB a = Parser Token a

tok :: Token -> SMTLIB ()
tok a = satisfy (==  a) >> return ()

left :: SMTLIB ()
left = tok LeftParen

right :: SMTLIB ()
right = tok RightParen

numeral :: SMTLIB Numeral
numeral = do
  a <- satisfy (\ a -> case a of { Numeral _ -> True; _ -> False })
  case a of
    Numeral a -> return a
    _ -> undefined

string :: SMTLIB String
string = do
  a <- satisfy (\ a -> case a of { String _ -> True; _ -> False })
  case a of
    String a -> return a
    _ -> undefined

symbol :: SMTLIB Symbol
symbol = do
  a <- satisfy (\ a -> case a of { Symbol _ -> True; _ -> False })
  case a of
    Symbol a -> return a
    _ -> undefined

keyword :: SMTLIB Keyword
keyword = do
  a <- satisfy (\ a -> case a of { Keyword _ -> True; _ -> False })
  case a of
    Keyword a -> return a
    _ -> undefined

b_value :: SMTLIB Bool
b_value = oneOf
  [ do { tok $ Symbol "true"; return True }
  , do { tok $ Symbol "false"; return False }
  ]

-- | Lazily parses an SMT-LIB command script.
parseScript :: String -> Script
parseScript s = fst $ runParser script $ lexSMTLIB s

-- | Lazily parses an SMT-LIB command responses.
parseResponses :: String -> [Command_response]
parseResponses s = fst $ runParser responses $ lexSMTLIB s

-- | Lazily parses an SMT-LIB theory declaration.
parseTheory :: String -> Theory_decl
parseTheory s = fst $ runParser theory_decl $ lexSMTLIB s

-- | Lazily parses an SMT-LIB logic.
parseLogic :: String -> Logic
parseLogic s = fst $ runParser logic $ lexSMTLIB s

-- | Checks the parsing of a command script.
checkScript :: FilePath -> IO Bool
checkScript file = do
  script <- readFile file
  let orig = clean script
      parsed = clean $ show $ parseScript script
  if orig == parsed then return True else do
    writeFile (file ++ ".fail") $ show $ parseScript script
    return False

-- | Checks the parsing of command responses.
checkResponses :: FilePath -> IO Bool
checkResponses file = do
  script <- readFile file
  let orig = clean script
      parsed = clean $ show $ parseResponses script
  if orig == parsed then return True else do
    writeFile (file ++ ".fail") $ show $ parseResponses script
    return False

clean :: String -> String
clean = filter (flip notElem " \t\r\n") . unlines . map (takeWhile (/= ';')) . lines

-- | Recursively searches current directory for *.smt2 files to test the parser.
checkParser :: IO ()
checkParser = do
  result <- checkDir "."
  if result
    then putStrLn "\nall tests passed\n"
    else putStrLn "\nTESTS FAILED\n"
  where
  checkDir :: FilePath -> IO Bool
  checkDir dir = getDirectoryContents dir >>= mapM checkFile >>= return . and
    where
    checkFile :: FilePath -> IO Bool
    checkFile file = do
      let f = dir ++ "/" ++ file
      a <- doesDirectoryExist f
      if (a && notElem file [".", ".."])
        then checkDir f
        else if (isSuffixOf ".smt2" f)
          then do
            putStr $ "testing file " ++ f ++ " ... "
            hFlush stdout
            pass <- checkScript f
            putStrLn (if pass then "pass" else "FAIL")
            hFlush stdout
            return pass
          else return True

  




