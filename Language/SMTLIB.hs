-- | Parsing and printing SMT-LIB.
module Language.SMTLIB
  (
  -- * AST
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
  , Gi_response
  , Cs_response
  , Ga_response
  , Proof
  , Gp_response
  , Guc_response
  , Valuation_pair
  , Gv_response
  , T_valuation_pair
  , Gta_response
  ) where

import Data.List hiding (group)
import Text.Printf

type Numeral      = Integer
type Symbol       = String
type Keyword      = String

data Spec_constant
  = Spec_constant_numeral     Numeral
  | Spec_constant_decimal     Rational
  | Spec_constant_hexadecimal Integer
  | Spec_constant_binary      [Bool]
  | Spec_constant_string      String

instance Show Spec_constant where
  show a = case a of
    Spec_constant_numeral     a -> show a
    Spec_constant_decimal     a -> show (realToFrac a :: Double)
    Spec_constant_hexadecimal a -> printf "#x%x" a
    Spec_constant_binary      a -> printf "#b%s" [ if a then '1' else '0' | a <- a ]
    Spec_constant_string      a -> show a

data S_expr
  = S_expr_constant Spec_constant
  | S_expr_symbol   Symbol
  | S_expr_keyword  Keyword
  | S_exprs         [S_expr]

instance Show S_expr where
  show a = case a of
    S_expr_constant a -> show a
    S_expr_symbol   a -> a
    S_expr_keyword  a -> a
    S_exprs         a -> group $ map show a

data Identifier
  = Identifier  Symbol
  | Identifier_ Symbol [Numeral]

instance Show Identifier where
  show a = case a of
    Identifier  a -> a
    Identifier_ a b -> group $ ["_", a] ++ map show b

data Sort
  = Sort_bool
  | Sort_identifier  Identifier
  | Sort_identifiers Identifier [Sort]

instance Show Sort where
  show a = case a of
    Sort_bool -> "Bool"
    Sort_identifier  a -> show a
    Sort_identifiers a b -> group $ show a : map show b

data Attribute_value
  = Attribute_value_spec_constant Spec_constant
  | Attribute_value_symbol        Symbol
  | Attribute_value_s_expr        [S_expr]

instance Show Attribute_value where
  show a = case a of
    Attribute_value_spec_constant a -> show a
    Attribute_value_symbol        a -> a
    Attribute_value_s_expr        a -> group $ map show a

data Attribute
  = Attribute        Keyword
  | Attribute_s_expr Keyword S_expr

instance Show Attribute where
  show a = case a of
    Attribute        a -> a
    Attribute_s_expr a b -> a ++ " " ++ show b

data Qual_identifier
  = Qual_identifier      Identifier
  | Qual_identifier_sort Identifier Sort

instance Show Qual_identifier where
  show a = case a of
    Qual_identifier      a -> show a
    Qual_identifier_sort a b -> group ["as", show a, show b]

data Var_binding
  = Var_binding Symbol Term

instance Show Var_binding where
  show a = case a of
    Var_binding a b -> group [a, show b]

data Sorted_var
  = Sorted_var Symbol Sort

instance Show Sorted_var where
  show a = case a of
    Sorted_var a b -> group [a, show b]

data Term
  = Term_spec_constant    Spec_constant
  | Term_qual_identifier  Qual_identifier
  | Term_qual_identifier_ Qual_identifier [Term]
  | Term_distinct         Term [Term]
  | Term_let              [Var_binding] Term
  | Term_forall           [Sorted_var] Term
  | Term_exists           [Sorted_var] Term
  | Term_attributes       Term [Attribute]

instance Show Term where
  show a = case a of
    Term_spec_constant    a -> show a
    Term_qual_identifier  a -> show a
    Term_qual_identifier_ a b -> group $ show a : map show b
    Term_distinct         a b -> group $ ["distinct", show a] ++ map show b
    Term_let              a b -> group $ ["let",    group $ map show a, show b]
    Term_forall           a b -> group $ ["forall", group $ map show a, show b]
    Term_exists           a b -> group $ ["exists", group $ map show a, show b]
    Term_attributes       a b -> group $ ["!", show a] ++ map show b

data Sort_symbol_decl
  = Sort_symbol_decl Identifier Numeral [Attribute]

instance Show Sort_symbol_decl where
  show a = case a of
    Sort_symbol_decl a b c -> group $ [show a, show b] ++ map show c

data Meta_spec_constant
  = Meta_spec_constant_numeral
  | Meta_spec_constant_decimal
  | Meta_spec_constant_string

instance Show Meta_spec_constant where
  show a = case a of
    Meta_spec_constant_numeral -> "NUMERAL"
    Meta_spec_constant_decimal -> "DECIMAL"
    Meta_spec_constant_string  -> "STRING"

data Fun_symbol_decl
  = Fun_symbol_decl_spec_constant      Spec_constant      Sort [Attribute]
  | Fun_symbol_decl_meta_spec_constant Meta_spec_constant Sort [Attribute]
  | Fun_symbol_decl                    Identifier [Sort] [Attribute]

instance Show Fun_symbol_decl where
  show a = case a of
    Fun_symbol_decl_spec_constant      a b c -> group $ [show a, show b] ++ map show c
    Fun_symbol_decl_meta_spec_constant a b c -> group $ [show a, show b] ++ map show c
    Fun_symbol_decl                    a b c -> group $ [show a] ++ map show b ++ map show c

data Par_fun_symbol_decl
  = Par_fun_symbol_decl Fun_symbol_decl
  | Par_fun_symbol_decl_symbols [Symbol] Identifier [Sort] [Attribute]

instance Show Par_fun_symbol_decl where
  show a = case a of
    Par_fun_symbol_decl a -> show a
    Par_fun_symbol_decl_symbols a b c d -> group ["par", group $ map show a, group $ [show b] ++ map show c ++ map show d]

data Theory_attribute
  = Theory_attribute_sorts [Symbol]
  | Theory_attribute_funs  [Par_fun_symbol_decl]
  | Theory_attribute_sorts_desc String
  | Theory_attribute_funs_desc  String
  | Theory_attribute_definition String
  | Theory_attribute_values     String
  | Theory_attribute_notes      String
  | Theory_attribute            Attribute

instance Show Theory_attribute where
  show a = case a of
    Theory_attribute_sorts      a -> ":sorts " ++ group (map show a)
    Theory_attribute_funs       a -> ":funs "  ++ group (map show a)
    Theory_attribute_sorts_desc a -> ":sorts-description " ++ show a
    Theory_attribute_funs_desc  a -> ":funs-description "  ++ show a
    Theory_attribute_definition a -> ":definition "        ++ show a
    Theory_attribute_values     a -> ":values "            ++ show a
    Theory_attribute_notes      a -> ":notes "             ++ show a
    Theory_attribute            a -> show a

data Theory_decl
  = Theory_decl Symbol [Theory_attribute]

instance Show Theory_decl where
  show a = case a of
    Theory_decl a b -> group $ ["theory", show a] ++ map show b

data Logic_attribute
  = Logic_attribute_theories   [Symbol]
  | Logic_attribute_language   String
  | Logic_attribute_extensions String
  | Logic_attribute_values     String
  | Logic_attribute_notes      String
  | Logic_attribute            Attribute

instance Show Logic_attribute where
  show a = case a of
    Logic_attribute_theories    a -> ":theories " ++ group (map show a)
    Logic_attribute_language    a -> ":language "   ++ show a
    Logic_attribute_extensions  a -> ":extensions " ++ show a
    Logic_attribute_values      a -> ":values "     ++ show a
    Logic_attribute_notes       a -> ":notes "      ++ show a
    Logic_attribute             a -> show a

data Logic
  = Logic Symbol [Logic_attribute]

instance Show Logic where
  show a = case a of
    Logic a b -> group $ ["logic", a] ++ map show b

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

instance Show Option where
  show a = case a of
    Print_success             a -> ":print-success "             ++ showBool a
    Expand_definitions        a -> ":expand-definitions "        ++ showBool a
    Interactive_mode          a -> ":interactive-mode "          ++ showBool a
    Produce_proofs            a -> ":produce-proofs "            ++ showBool a
    Produce_unsat_cores       a -> ":produce-unsat-cores "       ++ showBool a
    Produce_models            a -> ":produce-models "            ++ showBool a
    Produce_assignments       a -> ":produce-assignments "       ++ showBool a
    Regular_output_channel    a -> ":regular-output-channel "    ++ show a
    Diagnostic_output_channel a -> ":diagnostic-output-channel " ++ show a
    Random_seed               a -> ":random-seed "               ++ show a
    Verbosity                 a -> ":verbosity "                 ++ show a
    Option_attribute          a -> show a

data Info_flag
  = Error_behavior
  | Name
  | Authors
  | Version
  | Status
  | Reason_unknown
  | Info_flag Keyword
  | All_statistics

instance Show Info_flag where
  show a = case a of
    Error_behavior -> ":error-behavior"
    Name           -> ":name"
    Authors        -> ":authors"
    Version        -> ":version"
    Status         -> ":status"
    Reason_unknown -> ":reason-unknown"
    Info_flag    a -> a
    All_statistics -> ":all-statistics"

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

instance Show Command where
  show a = case a of
    Set_logic    a -> group ["set-logic", a]
    Set_option   a -> group ["set-option", show a]
    Set_info     a -> group ["set-info", show a]
    Declare_sort a b -> group ["declare-sort", a, show b]
    Define_sort  a b c -> group ["define-sort", a, group (map show b), show c]
    Declare_fun  a b c -> group ["declare-fun", a, group (map show b), show c]
    Define_fun   a b c d -> group ["define_fun", a, group (map show b), show c, show d]
    Push a -> group ["push", show a]
    Pop  a -> group ["pop",  show a]
    Assert a -> group ["assert", show a]
    Check_sat -> group ["check-sat"]
    Get_assertions -> group ["get-assertions"]
    Get_proof      -> group ["get-proof"]
    Get_unsat_core -> group ["get-unsat-core"]
    Get_value a -> group ["get-value", group $ map show a]
    Get_assignment -> group ["get-assignment"]
    Get_option a -> group ["get-option", a]
    Get_info   a -> group ["get-info", show a]
    Exit -> group ["exit"]

data Script = Script [Command]

instance Show Script where
  show (Script a) = unlines $ map show a

data Gen_response
  = Unsupported
  | Success
  | Error String

instance Show Gen_response where
  show a = case a of
    Unsupported  -> "unsupported"
    Success      -> "sucess"
    Error a      -> group ["error", show a]

data Error_behavior
  = Immediate_exit
  | Continued_execution

instance Show Error_behavior where
  show a = case a of
    Immediate_exit      -> "immediate-exit"
    Continued_execution -> "continued-execution"

data Reason_unknown
  = Timeout
  | Memout
  | Incomplete

instance Show Reason_unknown where
  show a = case a of
    Timeout    -> "timeout"
    Memout     -> "memout"
    Incomplete -> "incomplete"

data Status
  = Sat
  | Unsat
  | Unknown

instance Show Status where
  show a = case a of
    Sat     -> "sat"
    Unsat   -> "unsat"
    Unknown -> "unknown"

data Info_response
  = Info_response_error_behavior Error_behavior
  | Info_response_name    String
  | Info_response_authors String
  | Info_response_version String
  | Info_response_status  Status
  | Info_response_reason_unknown Reason_unknown
  | Info_response_attribute Attribute

instance Show Info_response where
  show a = case a of
    Info_response_error_behavior a -> ":error-behavior " ++ show a
    Info_response_name           a -> ":name "           ++ show a
    Info_response_authors        a -> ":authors "        ++ show a
    Info_response_version        a -> ":version "        ++ show a
    Info_response_status         a -> ":status "         ++ show a
    Info_response_reason_unknown a -> ":reason-unknown " ++ show a
    Info_response_attribute      a -> show a

type Gi_response      = [Info_response]
type Cs_response      = Status
type Ga_response      = [Term]
type Proof            = S_expr
type Gp_response      = Proof
type Guc_response     = [Symbol]
type Valuation_pair   = (Term, Term)
type Gv_response      = [Valuation_pair]
type T_valuation_pair = (Symbol, Bool)
type Gta_response     = [T_valuation_pair]

group :: [String] -> String
group a = "( " ++ intercalate " " a ++ " )"

showBool :: Bool -> String
showBool a = if a then "true" else "false"

