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
  , Script
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

type Numeral      = Integer
type Symbol       = String
type Keyword      = String

data Spec_constant
  = Spec_constant_numeral     Numeral
  | Spec_constant_decimal     Rational
  | Spec_constant_hexadecimal Integer
  | Spec_constant_binary      [Bool]
  | Spec_constant_string      String

data S_expr
  = S_expr_constant Spec_constant
  | S_expr_symbol   Symbol
  | S_expr_keyword  Keyword
  | S_exprs         [S_expr]

data Identifier
  = Identifier  Symbol
  | Identifier_ Symbol [Numeral]

data Sort
  = Sort_bool
  | Sort_identifier  Identifier
  | Sort_identifiers Identifier [Sort]

data Attribute_value
  = Attribute_value_spec_constant Spec_constant
  | Attribute_value_symbol        Symbol
  | Attribute_value_s_expr        [S_expr]

data Attribute
  = Attribute        Keyword
  | Attribute_s_expr Keyword S_expr

data Qual_identifier
  = Qual_identifier      Identifier
  | Qual_identifier_sort Identifier Sort

data Var_binding
  = Var_binding Symbol Term

data Sorted_var
  = Sorted_var Symbol Sort

data Term
  = Term_spec_constant    Spec_constant
  | Term_qual_identifier  Qual_identifier
  | Term_qual_identifier_ Qual_identifier [Term]
  | Term_distinct         Term [Term]
  | Term_let              [Var_binding] Term
  | Term_forall           [Sorted_var] Term
  | Term_exists           [Sorted_var] Term
  | Term_attributes       Term [Attribute]

data Sort_symbol_decl
  = Sort_symbol_decl Identifier Numeral [Attribute]

data Meta_spec_constant
  = Meta_spec_constant_numeral
  | Meta_spec_constant_decimal
  | Meta_spec_constant_string

data Fun_symbol_decl
  = Fun_symbol_decl_spec_constant      Sort [Attribute]
  | Fun_symbol_decl_meta_spec_constant Sort [Attribute]
  | Fun_symbol_decl                    Identifier [Sort] [Attribute]

data Par_fun_symbol_decl
  = Par_fun_symbol_decl Fun_symbol_decl
  | Par_fun_symbol_decl_symbols [Symbol]
  | Par_fun_symbol_decl_attribute Identifier [Sort] [Attribute]

data Theory_attribute
  = Theory_attribute_sorts [Symbol]
  | Theory_attribute_funs  [Par_fun_symbol_decl]
  | Theory_attribute_sorts_desc String
  | Theory_attribute_funs_desc  String
  | Theory_attribute_definition String
  | Theory_attribute_values     String
  | Theory_attribute_notes      String
  | Theory_attribute            Attribute

data Theory_decl
  = Theory_decl Symbol [Theory_attribute]

data Logic_attribute
  = Logic_attribute_theories   [Symbol]
  | Logic_attribute_language   String
  | Logic_attribute_extensions String
  | Logic_attribute_values     String
  | Logic_attribute_notes      String
  | Logic_attribute            Attribute

data Logic
  = Logic Symbol [Logic_attribute]

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
  | Random_seed Integer
  | Verbosity Integer
  | Option_attribute Attribute

data Info_flag
  = Error_behavior
  | Name
  | Authors
  | Version
  | Status
  | Reason_unknown
  | Info_flag Keyword
  | All_statistics

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
  | Get_option [Keyword]
  | Get_info Info_flag
  | Exit

type Script = [Command]

data Gen_response
  = Unsupported
  | Success
  | Error String

data Error_behavior
  = Immediate_exit
  | Continued_execution

data Reason_unknown
  = Timeout
  | Memout
  | Incomplete

data Status
  = Sat
  | Unsat
  | Unknown

data Info_response
  = Info_response_error_behavior Error_behavior
  | Info_response_name    String
  | Info_response_authors String
  | Info_response_version String
  | Info_response_status  Status
  | Info_response_reason_unknown Reason_unknown
  | Info_response_attribute Attribute

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

