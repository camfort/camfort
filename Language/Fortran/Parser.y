{
 {-# LANGUAGE QuasiQuotes #-}
 {-# LANGUAGE TypeSynonymInstances #-}
 {-# LANGUAGE FlexibleInstances #-}

module Language.Fortran.Parser  where

import Language.Fortran

import Language.Haskell.Syntax (SrcLoc,srcLine,srcColumn)
import Language.Haskell.ParseMonad 
import Language.Fortran.Lexer
import Data.Char (toLower)
-- import GHC.Exts

import Debug.Trace

}

%name parser
%tokentype { Token SrcLoc }

%monad { P } { >>= } { return }
%lexer { lexer } { TokEOF l }

%token
 '=>'			{ Arrow l }
 '**'			{ OpPower l }
 '//' 			{ OpConcat l }
 '=='		        { OpEQ l }
 '/='       		{ OpNE l }
 '<='		        { OpLE l }
 '>='		        { OpGE l }
 '.NOT.'		{ OpNOT l }
 '.AND.'		{ OpAND l }
 '.OR.'		        { OpOR l }
 '.TRUE.'		{ TrueConst l }
 '.FALSE.'		{ FalseConst l }
-- '.EQV.'		{ OpEQV l }
-- '.NEGV.' 	       	{ OpNEQV l }
 '<'		        { OpLT l }
 '>'		        { OpGT l }
 '*'		       	{ OpMul l }
 '/'		       	{ OpDiv l }
 '+'		       	{ OpAdd l }
 '-'		       	{ OpSub l }
 ','		       	{ Comma l }
 '('		       	{ LParen l }
 ')'		       	{ RParen l }
 '='		       	{ OpEquals l }
-- '\''		      	{ SingleQuote l }
-- '\"'			{ DoubleQuote l }
 '.'		        { Period l }
 '::'			{ ColonColon l }
 ':'			{ Colon l }
 ';'                    { SemiColon l }
 '#'                    { Hash l }
 '{'                    { LBrace l }
 '}'                    { RBrace l }
 '(/'                   { LArrCon l }
 '/)'                   { RArrCon l }
-- OBSOLETE '!'         { Bang l } 
 '%'			{ Percent l }
 '$'			{ Dollar l }
 -- OBSOLETE '!{'	{ StopParamStart l }
'\n'                    { NewLine l }
 ALLOCATE 		{ Key "allocate" l }
 ALLOCATABLE 		{ Key "allocatable" l }
-- ASSIGN 		{ Key "Assign" l }
 ASSIGNMENT 		{ Key "assignment" l }
-- AUTOMATIC 		{ Key "automatic" l }
 BACKSPACE 		{ Key "backspace" l }
 BLOCK 			{ Key "block" l }
 CALL 			{ Key "call" l }
-- CASE 		{ Key "case" l }
 CHARACTER 		{ Key "character" l }
 CLOSE 			{ Key "close" l }
 COMMON 		{ Key "common" l }
 COMPLEX 		{ Key "complex" l }
 CONTAINS 		{ Key "contains" l }
 CONTINUE 		{ Key "continue" l }
 CYCLE 			{ Key "cycle" l }
 DATA 			{ Key "data" l }
 DEALLOCATE 		{ Key "deallocate" l }
-- DEFAULT 		{ Key "default" l }
 DIMENSION 		{ Key "dimension" l }
 DO 			{ Key "do" l }
-- DOUBLE 		{ Key "double" l }
 ELEMENTAL 		{ Key "elemental" l }
 ELSE 			{ Key "else" l }
 ELSEIF 		{ Key "elseif" l }
-- ELSEWHERE 		{ Key "elsewhere" l }
 END 			{ Key "end" l }
 ENDIF			{ Key "endif" l }
 ENDDO			{ Key "enddo" l }
 ENDFILE                { Key "endfile" l }
-- ENTRY 		{ Key "entry" l }
 EQUIVALENCE 		{ Key "equivalence" l }
 EXIT 			{ Key "exit" l }
 EXTERNAL 		{ Key "external" l }
 FORALL 		{ Key "forall" l }
 FOREACH		{ Key "foreach" l }
-- FORMAT 		{ Key "format" l }
 FUNCTION 		{ Key "function" l }
 GOTO 			{ Key "goto" l }
 IOLENGTH               { Key "iolength" l }
 IF 			{ Key "if" l }
 IMPLICIT 		{ Key "implicit" l }
 IN 			{ Key "in" l }
 INCLUDE		{ Key "include" l }
 INOUT 			{ Key "inout" l }
 INTEGER 		{ Key "integer" l }
 INTENT 		{ Key "intent" l }
 INTERFACE 		{ Key "interface" l }
 INTRINSIC 		{ Key "intrinsic" l }
 INQUIRE 		{ Key "inquire" l }
 KIND 			{ Key "kind" l }
 LEN 			{ Key "len" l }
 LOGICAL 		{ Key "logical" l }
 MODULE 		{ Key "module" l }
 NAMELIST 		{ Key "namelist" l }
 NONE 			{ Key "none" l }
 NULLIFY 		{ Key "nullify" l }
 NULL 			{ Key "null" l }
-- ONLY 		{ Key "only" l }
 OPEN 			{ Key "open" l }
 OPERATOR 		{ Key "operator" l }
 OPTIONAL 		{ Key "optional" l }
 OUT 			{ Key "out" l }
 PARAMETER 		{ Key "parameter" l }
-- PAUSE 		{ Key "pause" l }
 POINTER 		{ Key "pointer" l }
-- PRECISION 		{ Key "precision" l }
 PRINT 			{ Key "print" l }
 PRIVATE 		{ Key "private" l }
 PROCEDURE 		{ Key "procedure" l }
 PROGRAM 		{ Key "program" l }
 PURE 			{ Key "pure" l }
 PUBLIC 		{ Key "public" l }
 REAL 			{ Key "real" l }
 READ 			{ Key "read" l }
 RECURSIVE 		{ Key "recursive" l }
 RESULT 		{ Key "result" l }
 RETURN 		{ Key "return" l }
 REWIND 		{ Key "rewind" l }
 SAVE 			{ Key "save" l }
-- SELECT 		{ Key "select" l }
 SEQUENCE 		{ Key "sequence" l }
-- SIZE 		{ Key "size" l }
 SOMETYPE               { Key "sometype" l }
 SQRT			{ Key "sqrt" l }
 STAT 			{ Key "stat" l }
 STOP			{ Key "stop" l }
 STR                    { StrConst $$ l }
 SUBROUTINE 		{ Key "subroutine" l }
 TARGET 		{ Key "target" l }
-- TO 			{ Key "to" l }
 THEN 			{ Key "then" l }
 TYPE 			{ Key "type" l }
-- UNFORMATED 		{ Key "unformatted" l }
 USE 			{ Key "use" l }
 VOLATILE 		{ Key "volatile" l }
 WHERE 			{ Key "where" l }
 WRITE 			{ Key "write" l }
 ID                     { ID $$ l }
 NUM                    { Num $$ l }
 TEXT                   { Text $$ l }
%%

executable_program :: { [Program A0] }
executable_program
  : program_unit_list                             { $1 }
    
program_unit_list :: { [Program A0] }
program_unit_list
  : program_unit_list newline0 program_unit       { $1++[$3] }
  | {- empty -}                                   { [] }

program_unit :: { Program A0 }
program_unit
  : main_program                                  { $1 }
  | external_subprogram                           { $1 }
  | module                                        { $1 }
  | block_data                                    { $1 }

plist :: { [String] }
plist 
  : plist ',' id2                                  { $1++[$3] }
  | id2                                            { [$1] }

vlist :: { [Expr A0] }
vlist 
  : variable ',' vlist                             { $1:$3 }
  | variable                                       { [$1] }

newline :: {}
newline : '\n' newline {}
        | '\n'         {}

newline0 :: {}
newline0 : newline    {} 
        | {- empty -} {}

main_program :: { Program A0 }
main_program
  : program_stmt use_stmt_list implicit_part specification_part_top execution_part module_subprogram_part end_program_stmt newline0
		{% do { s <- srcSpanFrom (fst $1);
		        name <- cmpNames (fst $1) $7 "program";
		        return (Main s name (snd $1) (Block s $2 $3 $4 $5) $6); } }

program_stmt :: { (SubName A0, Arg A0) }
program_stmt
  : PROGRAM subname args_p newline   { ($2, $3) }
  | PROGRAM subname        newline   {% (srcSpanFrom $1) >>= (\l -> return $ ($2, (Arg l (NullArg l)))) } 

end_program_stmt :: { String }
end_program_stmt
  : END PROGRAM id2   { $3 }
  | END PROGRAM       { "" }
  | END               { "" }

implicit_part :: { Implicit A0 }
implicit_part : IMPLICIT NONE newline {% (srcSpanFrom $1) >>= (return . ImplicitNone) }
              | {- empty -}           {% srcSpanNull >>= (return . ImplicitNull) }

--args
--  : args ',' id2                                   { }
--  | args                                          { }
--end_program_stmt :: { String }
--  : END                                           { "" }
--  | END PROGRAM                                   { "" }
--  | END PROGRAM id2                                { $3 }


external_subprogram :: { Program A0}
external_subprogram
  : function_subprogram                         { $1 }
  | subroutine_subprogram                       { $1 } 

subroutine_subprogram :: { Program A0 }
subroutine_subprogram 
  : subroutine_stmt use_stmt_list implicit_part specification_part_top execution_part end_subroutine_stmt newline0
  {% do { s <- srcSpanFrom (fst3 $1);
          name <- cmpNames (fst3 $1) $6 "subroutine";
          return (Sub s (trd3 $1) name (snd3 $1) (Block s $2 $3 $4 $5)); } }

end_subroutine_stmt :: { String }
end_subroutine_stmt
  : END SUBROUTINE id2          { $3 }
  | END SUBROUTINE              { "" }
  | END                         { "" }

end_function_stmt :: { String }
end_function_stmt
  : END FUNCTION id2            { $3 }
  | END FUNCTION                { "" }
  | END                         { "" }

function_subprogram :: { Program A0 }
function_subprogram
  : function_stmt use_stmt_list implicit_part specification_part_top execution_part end_function_stmt newline0  {% do { s <- srcSpanFrom (fst3 $1);
                        name <- cmpNames (fst3 $1) $6 "function";
                        return (Function s (trd3 $1) name (snd3 $1) (Block s $2 $3 $4 $5)); } }

block_data :: { Program A0 }
block_data
  : block_data_stmt use_stmt_list implicit_part specification_part_top end_block_data_stmt     
                  {% do { s <- srcSpanFrom $1;
                          name <- cmpNames $1 $5 "block data";
                          return (BlockData s name $2 $3 $4); } }
  
block_data_stmt :: { SubName A0 }
block_data_stmt
  : BLOCK DATA subname                     { $3 } 
  | BLOCK DATA                             {% do { s <- srcSpanFrom $1; 
                                                   return $ NullSubName s; } } 

end_block_data_stmt :: { String }
end_block_data_stmt
  : END BLOCK DATA id2                            { $4 }
  | END BLOCK DATA                                { "" }
  | END                                           { "" }
  
module :: { Program A0 }
module
  : module_stmt use_stmt_list implicit_part specification_part_top module_subprogram_part end_module_stmt {% do { s <- srcSpanFrom $1;
                   name <- cmpNames $1 $6  "module";
                   return (Module s name $2 $3 $4 $5); } }

module_stmt :: { SubName A0 }
module_stmt
  : MODULE subname newline                    { $2 } 

end_module_stmt :: { String }
end_module_stmt
  : END MODULE id2                            { $3 }
  | END MODULE                                { "" }
  | END                                       { "" }

module_subprogram_part :: { [Program A0] }
module_subprogram_part
  : CONTAINS newline internal_subprogram_list { $3 }
| {- empty -}                                 { [] } 
  
internal_subprogram_list :: { [Program A0] }
internal_subprogram_list
  : internal_subprogram_list internal_subprogram newline0 { $1++[$2] } 
  | {- empty -}                                           { [] }
  
internal_subprogram :: { Program A0 }
internal_subprogram
  : subroutine_subprogram                           { $1 }
  | function_subprogram                             { $1 }
  
use_stmt_list :: { [String] }
use_stmt_list
  : use_stmt_list use_stmt  { $2:$1 }
 | {- empty -}  	    { [] }

use_stmt :: { String }
use_stmt
  : USE id2 newline { $2 }
  
-- [DO: Allows the specification part of a module to be empty]
specification_part_top :: { Decl A0 }
specification_part_top
   : specification_part   { $1 }
   |  {- empty -}         {% srcSpanNull >>= (return . NullDecl) }

specification_part :: { Decl A0 }
specification_part
  : declaration_construct_l specification_part {% srcSpanFromL $1 (\s -> DSeq s $1 $2) }
  | declaration_construct_l                    { $1 }
  

declaration_construct_l :: { Decl A0 }
declaration_construct_l
  : declaration_construct_p newline { $1 }

declaration_construct_p :: { Decl A0 }
declaration_construct_p
  : declaration_construct                         { $1 }
  | specification_stmt                            { $1 }
  | derived_type_def                              { $1 }
  | TEXT					  {% srcSpan l >>= (\s -> return $ TextDecl s $1) }

declaration_construct :: { Decl A0 }
declaration_construct
  : type_spec_p attr_spec_list '::' entity_decl_list  {% srcSpanFromL (fst3 $1) (\s ->
                                                          if isEmpty (fst $2) 
                                                          then Decl s $4 ((BaseType s (fst3 $1) (snd $2) (snd3 $1) (trd3 $1)))
							  else Decl s $4 ((ArrayT s  (fst $2) (fst3 $1) (snd $2) (snd3 $1) (trd3 $1)))) }
  | type_spec_p attr_spec_list      entity_decl_list  {% srcSpanFromL (fst3 $1) (\s ->
                                                          if isEmpty (fst $2) 
                                                          then Decl s $3 ((BaseType s (fst3 $1) (snd $2) (snd3 $1) (trd3 $1)))
							  else Decl s $3 ((ArrayT   s (fst $2) (fst3 $1) (snd $2) (snd3 $1) (trd3 $1)))) }
  | interface_block				      { $1 }
  | include_stmt { $1 }

attr_spec_list :: {([(Expr A0, Expr A0)],[Attr A0])}
attr_spec_list
  : attr_spec_list ',' attr_spec                  { (fst $1++fst $3,snd $1++snd $3) }
  | {- empty -}                                   { ([],[]) }

entity_decl_list :: { [(Expr A0, Expr A0)] }
entity_decl_list
  : entity_decl_list ',' entity_decl              { $1++[$3] }
  | entity_decl                                   { [$1] }

entity_decl :: { (Expr A0, Expr A0) }
entity_decl
  : ID '=' expr      {% srcSpan l >>= (\s -> return $ (Var s [(VarName s $1,[])], $3)) }
  | variable         {% srcSpanNull >>= (\s -> return ($1, NullExpr s)) }   -- TODO too general need to eleminate ability to parse v%u type vars
--  | function_name [ '*' char_length ]

object_name :: { String }
object_name
  : id2                                            { $1 }

type_spec_p :: { (BaseType A0, Expr A0, Expr A0) }
type_spec_p
  : type_spec                                     { (fst3 $1, snd3 $1, trd3 $1) }

type_spec :: { (BaseType A0, Expr A0, Expr A0) }
type_spec
  : INTEGER kind_selector                         {% srcSpanFromL $1 (\s -> (Integer s, $2, ne s)) }
  | INTEGER '*' length_value                      {% srcSpanFromL $1 (\s -> (Integer s, $3,ne s)) }
  | INTEGER                                       {% srcSpanFromL $1 (\l ->(Integer l,(ne l),ne l)) }
  | REAL kind_selector                            {% srcSpanFromL $1 (\l -> (Real l, $2, ne l)) }
  | REAL '*' length_value                         {% srcSpanFromL $1 (\l -> (Real l,$3,ne l)) }
  | REAL                                          {% srcSpanFromL $1 (\l -> (Real l,(ne l),ne l)) }
  | SOMETYPE                                      {% srcSpanFromL $1 (\l -> (SomeType l,(ne l),ne l)) }
--  | DOUBLE PRECISION kind_selector                { (Double,$3,ne l) }
--  | DOUBLE PRECISION '*' length_value             { (Double,$4,ne l) }
--  | DOUBLE PRECISION                              { (Double,ne l,ne l) }
  | COMPLEX kind_selector                         {% srcSpanFromL $1 (\l -> (Complex l,$2,ne l)) }
  | COMPLEX '*' length_value                      {% srcSpanFromL $1 (\l -> (Complex l,$3,ne l)) }
  | COMPLEX                                       {% srcSpanFromL $1 (\l -> (Complex l,ne l,ne l)) }
  | CHARACTER char_selector                       {% srcSpanFromL $1 (\l -> (Character l,snd $2, fst $2)) }
  | CHARACTER                                     {% srcSpanFromL $1 (\l -> (Character l,ne l,ne l)) }
  | LOGICAL kind_selector                         {% srcSpanFromL $1 (\l -> (Logical l,$2,ne l)) }
  | LOGICAL '*' length_value                      {% srcSpanFromL $1 (\l -> (Logical l,$3,ne l)) }
  | LOGICAL                                       {% srcSpanFromL $1 (\l -> (Logical l,ne l,ne l)) }
  | TYPE '(' type_name ')'                        {% srcSpanFromL $1 (\l -> (DerivedType l $3,ne l,ne l)) }
--  | POINTER '(' pointer_name ',' pointee_name ['(' array_spec ')' ] ')'
--[',' '(' pointer_name ',' pointee_name ['(' array_spec ')' ] ')' ] ...

kind_selector :: { Expr A0}
  : '(' KIND '=' expr ')'                         { $4 }
  | '(' expr ')'                                  { $2 }

char_selector :: { (Expr A0, Expr A0) }  -- (LEN, KIND)
char_selector 
: length_selector                                         {% srcSpanNull >>= (\s -> return $ ($1, ne s)) }
| '(' LEN '=' char_len_param_value ',' KIND '=' expr ')'  { ($4,$8) }
| '(' char_len_param_value ',' KIND '=' expr ')'          { ($2,$6) }
| '(' char_len_param_value ',' expr ')'                   {% srcSpanNull >>= (\s -> return ($2, ne s)) }
| '(' KIND '=' expr ',' LEN '=' char_len_param_value ')'  { ($8,$4) }
| '(' KIND '=' expr ')'                                   {% srcSpanNull >>= (\s -> return (ne s, $4)) }

length_selector :: { Expr A0 }
length_selector 
: '(' LEN '=' char_len_param_value ')'                                    { $4 }
| '(' char_len_param_value ')'                                            { $2 }

char_len_param_value :: { Expr A0 }
char_len_param_value
  : specification_expr                                     { $1 }
  | '*'                                                    {% srcSpanFromL $1 (\s -> Con s "*") }

length_value :: { Expr A0 }
length_value
  : NUM                                           {% srcSpan l >>= (\s -> return $ Con s $1) }

dim_spec :: { [(Expr A0, Expr A0)] }
dim_spec
  : DIMENSION '(' array_spec ')' { $3 }
  | DIMENSION '(' ')'            { [] }  -- modified by Zhe on 11/14/2004

attr_spec :: { ([(Expr A0, Expr A0)],[Attr A0]) }
attr_spec
  : dim_spec                                       { ($1,[]) }
  | PARAMETER                                      {% srcSpanFromL $1 (\s -> ([],[Parameter s])) }
  | access_spec                                    { ([],[$1]) }
  | ALLOCATABLE                                    {% srcSpanFromL $1 (\s -> ([],[Allocatable s ])) }
  | EXTERNAL                                       {% srcSpanFromL $1 (\s -> ([],[External s])) }
  | INTENT '(' intent_spec ')'                     {% srcSpanFromL $1 (\s -> ([],[Intent s $3])) }
  | INTRINSIC                                      {% srcSpanFromL $1 (\s -> ([],[Intrinsic s])) }
  | OPTIONAL                                       {% srcSpanFromL $1 (\s -> ([],[Optional s])) }
  | POINTER                                        {% srcSpanFromL $1 (\s -> ([],[Pointer s])) }
  | SAVE                                           {% srcSpanFromL $1 (\s -> ([],[Save s])) }
  | TARGET                                         {% srcSpanFromL $1 (\s -> ([],[Target s])) }
  | VOLATILE                                       {% srcSpanFromL $1 (\s -> ([],[Volatile s])) }

access_spec :: { Attr A0 }
access_spec
  : PUBLIC            {% srcSpanFromL $1 (\s -> Public s) }
  | PRIVATE           {% srcSpanFromL $1 (\s -> Private s) }

array_spec :: { [(Expr A0, Expr A0)] }
array_spec
  : explicit_shape_spec_list                      { map expr2array_spec $1 }


explicit_shape_spec_list :: { [Expr A0] }
explicit_shape_spec_list
  : explicit_shape_spec_list ','  explicit_shape_spec {$1++[$3]}
  | explicit_shape_spec                               {[$1]}
explicit_shape_spec :: { Expr A0 }
explicit_shape_spec
  : expr  { $1 } 
  | bound { $1 }

include_stmt :: { Decl A0 }
  : INCLUDE STR               {% do { s1 <- srcSpanFrom $1;
                                      s2 <- srcSpan l;
                                      return $ Include s1 (Con s2 $2); } }

specification_expr :: { Expr A0 }
specification_expr
  : expr { $1 } 
intent_spec :: { IntentAttr A0 }
intent_spec
  : IN            {% srcSpanFromL $1 (\s -> In s) }
  | OUT           {% srcSpanFromL $1 (\s -> Out s) }
  | INOUT         {% srcSpanFromL $1 (\s -> InOut s) }

specification_stmt :: { Decl A0 }
specification_stmt
  : access_stmt            { $1 }
--  | allocatable_stmt       { $1 }
  | common_stmt            { $1 }
  | data_stmt              { $1 }
--  | dimension_stmt         { $1 }
  | external_stmt          { $1 }
--  | intent_stmt            { $1 }
--  | intrinsic_stmt         { $1 }
  | namelist_stmt            { $1 }
--  | optional_stmt          { $1 }
--  | pointer_stmt           { $1 }
--  | save_stmt              { $1 }
--  | target_stmt            { $1 }

common_stmt :: { Decl A0 }
 : COMMON '/' id2 '/' vlist  {% srcSpanFromL $1 (\s -> Common s (Just $3) $5) }
 | COMMON vlist              {% srcSpanFromL $1 (\s -> Common s Nothing $2) }


interface_block :: { Decl A0 }
interface_block
  : interface_stmt interface_spec_list end_interface_stmt  {% case $1 of 
                                                                Nothing -> srcSpanNull >>= (\s -> return $ Interface s $1 $2)
                                                                Just y -> srcSpanFromL y (\s -> Interface s $1 $2) }

interface_stmt :: { Maybe (GSpec A0) }
interface_stmt
  : INTERFACE generic_spec       { Just $2 }
  | INTERFACE                    { Nothing }
  
interface_spec_list :: { [InterfaceSpec A0] }
interface_spec_list
  : interface_spec_list interface_spec   { $1++[$2] }
  | interface_spec                       { [$1] }
  
interface_spec :: { InterfaceSpec A0 }
interface_spec
  : interface_body               { $1 }
  | module_procedure_stmt        { $1 }
  
end_interface_stmt :: { Maybe (GSpec A0) }
end_interface_stmt
  : END INTERFACE generic_spec       { Just $3 }
  | END INTERFACE                    { Nothing }

interface_body :: { InterfaceSpec A0 } 
interface_body
  : function_stmt  use_stmt_list implicit_part specification_part end_function_stmt 
        {% do { s <- srcSpanFrom (fst3 $1);
                name <- cmpNames (fst3 $1) $5 "interface declaration";
                return (FunctionInterface s  name (snd3 $1) $2 $3 $4); }}

  | function_stmt end_function_stmt  
        {% do { s <- srcSpanFrom (fst3 $1);
                name <- cmpNames (fst3 $1) $2 "interface declaration";
                return (FunctionInterface s name (snd3 $1) [] (ImplicitNull s) (NullDecl s)); } }       

  | subroutine_stmt use_stmt_list implicit_part specification_part end_subroutine_stmt
        {% do { s <- srcSpanFrom (fst3 $1);
                name <- cmpNames (fst3 $1) $5 "interface declaration";
                return (SubroutineInterface s name (snd3 $1) $2 $3 $4); } }
  | subroutine_stmt end_subroutine_stmt 
        {% do { s <- srcSpanFrom (fst3 $1);
                name <- cmpNames (fst3 $1) $2 "interface declaration";
                return (SubroutineInterface s name (snd3 $1) [] (ImplicitNull s) (NullDecl s)); }}
  
module_procedure_stmt :: { InterfaceSpec A0 }
module_procedure_stmt
  : MODULE PROCEDURE sub_name_list    {% srcSpanFromL $1 (\s -> ModuleProcedure s $3 ) }

sub_name_list :: { [SubName A0 ] }
sub_name_list
  :  sub_name_list ',' sub_name     { $1++[$3] }
  |  sub_name     { [$1] }

sub_name :: { SubName A0 }
sub_name
  :  ID     {% srcSpan l >>= (\s -> return $ SubName s $1) }

derived_type_def :: { Decl A0 }
derived_type_def
  : derived_type_stmt private_sequence_stmt component_def_stmt_list end_type_stmt
  {% do { s <- srcSpanFrom (fst $1);
          name <- cmpNames (fst $1) $4 "derived type name";
          return (DerivedTypeDef s name (snd $1) $2 $3);  } }

derived_type_stmt :: { (SubName A0, [Attr A0]) }
derived_type_stmt
  : TYPE ',' access_spec  '::' type_name         { ($5,[$3]) }
  | TYPE                  '::' type_name         { ($3,[]) }
  | TYPE                       type_name         { ($2,[]) }

end_type_stmt :: { String }
end_type_stmt
  : END TYPE       { "" }
  | END TYPE id2   { $3 }


type_name :: { SubName A0 }
type_name
  : ID           {% srcSpan l >>= (\s -> return $ SubName s $1) } 

private_sequence_stmt :: { [Attr A0] }
private_sequence_stmt
  : PRIVATE SEQUENCE     {% do { s1 <- srcSpanFrom $1; 
                                 s2 <- srcSpanFrom $2;
                                 return [Private s1, Sequence s2]; } }
  | SEQUENCE PRIVATE     {% do { s1 <- srcSpanFrom $1; 
                                 s2 <- srcSpanFrom $2;
                                 return [Sequence s1, Private s2]; } }
  | PRIVATE              {% srcSpanFromL $1 (\s -> [Private s]) }
  | SEQUENCE             {% srcSpanFromL $1 (\s -> [Sequence s]) }
  | {- empty -}          { [] }
  
component_def_stmt_list :: { [Decl A0 ] }
component_def_stmt_list
  : component_def_stmt_list component_def_stmt     { $1++[$2] }
  | component_def_stmt                             { [$1] }

component_def_stmt :: { Decl A0 }
component_def_stmt
  : type_spec_p component_attr_spec_list '::' entity_decl_list  
        {% srcSpanFromL (fst3 $1) (\s -> if isEmpty (fst $2) 
                              then Decl s $4 ((BaseType s (fst3 $1) (snd $2) (snd3 $1) (trd3 $1)))
			      else Decl s $4 ((ArrayT s (fst $2) (fst3 $1) (snd $2) (snd3 $1) (trd3 $1)))) }

component_attr_spec_list :: {([(Expr A0, Expr A0)],[Attr A0])}
component_attr_spec_list
  : component_attr_spec_list ',' component_attr_spec       { (fst $1++fst $3,snd $1++snd $3) }
  | {- empty -}                                            { ([],[]) }

component_attr_spec :: { ([(Expr A0, Expr A0)],[Attr A0]) }
component_attr_spec
  :  POINTER              {% srcSpanFromL $1 (\s -> ([],[Pointer s])) }
  | dim_spec              { ($1,[]) }

access_stmt :: { Decl A0 }
access_stmt
  : access_spec '::' access_id_list  {% srcSpanFromL $1 (\s -> AccessStmt s $1 $3) }
  | access_spec access_id_list       {% srcSpanFromL $1 (\s -> AccessStmt s $1 $2) }
  | access_spec                      {% srcSpanFromL $1 (\s -> AccessStmt s $1 []) }
   
access_id_list :: { [GSpec A0] }
access_id_list
  : access_id_list ',' access_id     { $1++[$3] }
  | access_id                        { [$1] }

access_id :: { GSpec A0 }
access_id 
  : generic_spec                     { $1 }
  
generic_spec :: { GSpec A0 }
generic_spec
: ID					{% srcSpan l >>= (\s -> return $ GName s (Var s [(VarName s $1,[])])) } 
  | OPERATOR '(' defined_operator ')'   {% srcSpanFromL $1 (\s -> GOper s $3) }
  | ASSIGNMENT '(' '=' ')'              {% srcSpanFromL $1 (\s -> GAssg s) }
  
data_stmt :: { Decl A0 }
data_stmt
  : DATA data_stmt_set_list				{% srcSpanFromL $1 (\s ->(Data s $2)) }
  
data_stmt_set_list :: { [(Expr A0, Expr A0)] }
data_stmt_set_list
  : data_stmt_set_list ',' data_stmt_set	{ $1++[$3] }
  | data_stmt_set        		   	{ [$1] }
  
data_stmt_set :: { (Expr A0, Expr A0) }
data_stmt_set
  : data_stmt_object_list '/' data_stmt_value_list '/'		{ ($1,$3) }

data_stmt_object_list :: { Expr A0 }
data_stmt_object_list
  : data_stmt_object_list ',' data_stmt_object   {% srcSpanFromL $1 (\s ->ESeq s $1 $3) }
  | data_stmt_object			         { $1 }

data_stmt_object :: { Expr A0 }
data_stmt_object
  : variable 			{ $1 }
  

data_stmt_value_list :: { Expr A0 }
data_stmt_value_list
  : data_stmt_value_list ',' data_stmt_value	{% srcSpanFromL $1 (\s -> ESeq s $1 $3) }
  | data_stmt_value				{ $1 }

data_stmt_value :: { Expr A0 }
data_stmt_value
  : primary			{ $1 }
  
  
external_stmt :: { Decl A0 }
external_stmt
  : EXTERNAL '::' name_list  {% srcSpanFromL $1 (\s -> ExternalStmt s $3) }
  | EXTERNAL      name_list  {% srcSpanFromL $1 (\s -> ExternalStmt s $2) }
  
name_list :: { [String] }
name_list
  : name_list ',' id2          { $1++[$3] }
  | id2                        { [$1] }

id2 :: { String } -- hack len
id2 : ID  { $1 }
    | LEN { "len" }
	
defined_operator :: { BinOp A0 }
defined_operator
--  : defined_binary_op
--  | defined_unary_op
  : intrinsic_operator { $1 }

intrinsic_operator :: { BinOp A0 }
intrinsic_operator
  : '**'        {% srcSpanFromL $1 Power }
  | '*'         {% srcSpanFromL $1 Mul }
  | '+'         {% srcSpanFromL $1 Plus }
  | '//'        {% srcSpanFromL $1 Concat }
  | rel_op      { $1 }
--  | '.NOT.'     { Not }
  | '.AND.'     {% srcSpanFromL $1 And }
  | '.OR.'      {% srcSpanFromL $1 Or } 
--  | equiv_op    { 



namelist_stmt :: { Decl A0 }
namelist_stmt
  : NAMELIST namelist_list   {% srcSpanFromL $1 (\s -> Namelist s $2) }
  
namelist_list :: { [(Expr A0, [Expr A0])] }
namelist_list
  : namelist_list ',' '/' constant_p '/' namelist_group_object_list   { $1++[($4,$6)] }
  | '/' constant_p '/' namelist_group_object_list                     { [($2,$4)] }

namelist_group_object_list :: { [Expr A0] }
namelist_group_object_list
  : namelist_group_object_list ',' constant_p    { $1++[$3] }
  | constant_p                                   { [$1] }
  
subroutine_stmt :: { (SubName A0, Arg A0, Maybe (BaseType A0)) }
subroutine_stmt
  : SUBROUTINE subname args_p        newline { ($2,$3,Nothing) }
  | prefix SUBROUTINE subname args_p newline { ($3,$4,Just (fst3 $1)) }
  
function_stmt :: { (SubName A0, Arg A0, Maybe (BaseType A0)) }
function_stmt
  : prefix FUNCTION subname args_p RESULT '(' id2 ')' newline { ($3,$4,Just (fst3 $1)) }
  | prefix FUNCTION subname args_p                    newline { ($3,$4,Just (fst3 $1)) }
  | FUNCTION subname args_p RESULT '(' id2 ')'        newline { ($2,$3,Nothing) }
  | FUNCTION subname args_p                           newline { ($2,$3,Nothing) }
  
subname :: { SubName A0 }
subname
  : ID	   {% srcSpan l >>= (\s -> return $ SubName s $1) }
  
prefix :: { (BaseType A0, Expr A0, Expr A0) }
prefix
  : type_spec  { $1 }
  | RECURSIVE  {% srcSpanFromL $1 (\s -> (Recursive s, ne s, ne s)) }
  | PURE       {% srcSpanFromL $1 (\s -> (Pure s, ne s, ne s)) }
  | ELEMENTAL  {% srcSpanFromL $1 (\s -> (Elemental s, ne s, ne s)) }

args_p :: { Arg A0 }
args_p
  : '(' dummy_arg_list ')' { $2 }

dummy_arg_list :: { Arg A0 }
dummy_arg_list
  : dummy_arg_list2        {% srcSpanFromL $1 (\s -> Arg s $1) }
  | {- empty -}            {% srcSpanNull >>= (\s -> return $ Arg s (NullArg s)) }

dummy_arg_list2 :: { ArgName A0 } 
dummy_arg_list2
  : dummy_arg_list2 ',' dummy_arg                 {% srcSpanFromL $1 (\s -> ASeq s $1 $3) }
  | dummy_arg                                     { $1 }

dummy_arg :: { ArgName A0 }
dummy_arg
  : ID                              {% srcSpan l >>= (\s -> return $ ArgName s $1) }
  | '*'                             {% srcSpanFromL $1 (\s ->  ArgName s "*") }
  
--end_subroutine_stmt
--  : END SUBROUTINE

assignment_stmt :: { Fortran A0 }
assignment_stmt
  : variable '=' expr                                 {% srcSpanFromL $1 (\s -> Assg s $1 $3) }
 | ID '(' section_subscript_list ')' '=' expr         {% srcSpan l >>= (\s -> return $ Assg s (Var s [(VarName s $1, $3)]) $6) }


-- moved up to assignment_stmt
variable :: { Expr A0 }
variable
  : subobject                                  { $1 }

subobject :: { Expr A0 }
subobject
  : part_ref                                   { $1 }

part_ref :: { Expr A0 }
part_ref
  : scalar_variable_name_list                  {% srcSpanFromL (fst . head $ $1) (\s -> Var s $1) }

scalar_variable_name :: { (VarName A0, [Expr A0]) }
scalar_variable_name
  : ID	'(' section_subscript_list ')'   {% srcSpan l >>= (\s -> return $ (VarName s $1,$3)) }
  | ID '(' ')'                           {% srcSpan l >>= (\s -> return $ (VarName s $1,[ne s])) }
  | ID                                   {% srcSpan l >>= (\s -> return $ (VarName s $1,[])) }
  
scalar_variable_name_list :: { [(VarName A0, [Expr A0])] }
scalar_variable_name_list
  : scalar_variable_name_list '%' scalar_variable_name    { $1++[$3] }
  | scalar_variable_name                                  { [$1] }

--part_name :: { VarName }
--  : ID                                            { VarName $1 }

-- bound comes through int_expr
subscript :: { Expr A0 }
subscript
  : int_expr                                      { $1 }
  | bound                                         { $1 }
bound :: { Expr A0 }
bound
  : expr ':' expr                               {% srcSpanFromL $1 (\s -> Bound s $1 $3) }
  | expr ':'                                    {% srcSpanFromL $1 (\s -> Bound s $1 (ne s))}
  | ':' expr                                    {% srcSpanFromL $1 (\s -> Bound s (NullExpr s) $2) }
--  | ':'                                         { (Bound ne ne) }

section_subscript_list :: { [Expr A0] }
section_subscript_list
  : section_subscript_list ',' section_subscript  { $1++[$3] }
  | section_subscript                             { [$1] }
  
section_subscript :: { Expr A0 }
section_subscript
  : subscript                             { $1 }
  | ID '=' expr			          {% srcSpan l >>= (\s -> return $ AssgExpr s $1 $3) }
--  | subscript_triplet
--  | vector_subscript                              { $1 }

--subscript_triplet
--subscript_triplet
--  : [ subscript ] ':' [ subscript ] [ ':' stride ]

stride :: { Expr A0 }
stride 
  : int_expr                                      { $1 }

--vector_subscript :: { Expr A0 }
--vector_subscript
--  : int_expr                                      { $1 }



expr :: { Expr A0 }
expr
  : level_5_expr                                       { $1 }


level_5_expr :: { Expr A0 }
level_5_expr
  : equiv_operand                                      { $1 }

equiv_operand :: { Expr A0 }
equiv_operand
  : equiv_operand '.OR.' or_operand                    {% do { s1 <- srcSpanFrom $1;
                                                               s2 <- srcSpanFrom $2;
                                                               return $ Bin s1 (Or s2) $1 $3; } }
  | or_operand                                         { $1 }

or_operand :: { Expr A0 }
or_operand
  : or_operand '.AND.' and_operand                     {% do { s1 <- srcSpanFrom $1;
                                                               s2 <- srcSpanFrom $2;
                                                               return $ Bin s1 (And s2) $1 $3; } }
  | and_operand                                        { $1 }


and_operand :: { Expr A0 }
and_operand
  : level_4_expr                                       { $1 }

level_4_expr :: { Expr A0 }
level_4_expr 
  : level_4_expr rel_op level_3_expr                   {% srcSpanFromL $1 (\s -> Bin s $2 $1 $3) }
  | level_3_expr                                       { $1 }


level_3_expr :: { Expr A0 }
level_3_expr 
  : level_3_expr '//' level_2_expr                     {% do { s1 <- srcSpanFrom $1;
                                                               s2 <- srcSpanFrom $2;
                                                               return $ Bin s1 (Concat s2) $1 $3; } }
  | level_2_expr                                       { $1 }

level_2_expr :: { Expr A0 }
level_2_expr 
  : level_2_expr '+' add_operand                       {% do { s1 <- srcSpanFrom $1;
                                                               s2 <- srcSpanFrom $2;
                                                               return $ Bin s1 (Plus s2) $1 $3; } }
  | level_2_expr '-' add_operand                       {% do { s1 <- srcSpanFrom $1;
                                                               s2 <- srcSpanFrom $2;
                                                               return $ Bin s1 (Minus s2) $1 $3; } }
  | add_operand                                        { $1 }

add_operand :: { Expr A0 }
add_operand 
  : add_operand '*' mult_operand                       {% do { s1 <- srcSpanFrom $1;
                                                               s2 <- srcSpanFrom $2;
                                                               return $ Bin s1 (Mul s2) $1 $3; } }
  | add_operand '/' mult_operand                       {% do { s1 <- srcSpanFrom $1;
                                                               s2 <- srcSpanFrom $2;
                                                               return $ Bin s1 (Div s2) $1 $3; } }
  | mult_operand                                       { $1 }

mult_operand :: { Expr A0 }
mult_operand 
  : level_1_expr '**' mult_operand                     {% do { s1 <- srcSpanFrom $1;
                                                               s2 <- srcSpanFrom $2;
                                                               return $ Bin s1 (Power s2) $1 $3; } }
  | level_1_expr                                       { $1 }

level_1_expr :: { Expr A0 }
level_1_expr 
  : '-' primary                                        {% do { s1 <- srcSpanFrom $1;
                                                               return $ Unary s1 (UMinus s1) $2; } }
  | '.NOT.' primary                                    {% do { s1 <- srcSpanFrom $1;
                                                               return $ Unary s1 (Not s1) $2; } }
  | primary                                            { $1 }

primary :: { Expr A0 }
primary 
  : constant                                    { $1 }
  | variable                                    { $1 }
  | array_constructor                           { $1 }
  | '(' expr ')'                                { $2 }
  | SQRT '(' expr ')'				{% srcSpanFromL $1 (\s -> Sqrt s $3) }
  | ':'                                         {% srcSpanFromL $1 (\s -> Bound s (NullExpr s) (NullExpr s)) }
-- causes problems
--  |  function_reference                          { $1 }

fields :: { [String] }
fields
  : fields '.' id2                              { $1++[$3] }
  | id2                                         { [$1] }
  
array_constructor :: { Expr A0 }
array_constructor
  : '(/' expr_list '/)'           {% srcSpanFromL $1 (\s -> ArrayCon s $2) } 

expr_list :: { [Expr A0] }
expr_list
  : expr_list ',' expr          { $1++[$3] }
  | expr                        { [$1] }
  
constant_p :: { Expr A0 }
constant_p
  : constant_p2                        { $1 }
 
constant_p2 :: { Expr A0 }
constant_p2
  : ID             {% srcSpan l >>= (\s -> return $ Var s [(VarName s $1,[])]) }
  
constant :: { Expr A0 }
constant 
  : literal_constant                             { $1 }

literal_constant :: { Expr A0 }
literal_constant 
  : NUM                           {% (srcSpan l) >>= (\s -> return $ Con s $1) }
  | STR				  {% (srcSpan l) >>= (\s -> return $ ConS s $1) }
  | logical_literal_constant	  { $1 }

logical_literal_constant :: { Expr A0 }
logical_literal_constant 
  : '.TRUE.'                          {% srcSpanFromL $1 (\s -> Con s  ".TRUE.") }
  | '.FALSE.'                         {% srcSpanFromL $1 (\s -> Con s ".FALSE.") }


rel_op :: { BinOp A0 }
  : '=='                           {% srcSpanFromL $1 RelEQ }
  | '/='                           {% srcSpanFromL $1 RelNE }
  | '<'                            {% srcSpanFromL $1 RelLT }
  | '<='                           {% srcSpanFromL $1 RelLE }
  | '>'                            {% srcSpanFromL $1 RelGT }
  | '>='                           {% srcSpanFromL $1 RelGE }

int_expr :: { Expr A0 }
int_expr
  : expr                                         { $1 }

do_variable :: { VarName A0 } 
do_variable
  : ID                                    {% srcSpan l >>= (\s -> return $ VarName s $1) }

do_construct :: { Fortran A0 }
do_construct
  : block_do_construct                           { $1 }

block_do_construct :: { Fortran A0 } 
block_do_construct                         -- For  VarName Expr A0 Expr A0 Fortran
  : do_stmt do_block end_do  {% srcSpanFromL ((\(x, _, _, _) -> x) $1) (\s -> For s (fst4 $1) (snd4 $1) (trd4 $1) (frh4 $1) $2) }

do_stmt :: { (VarName A0, Expr A0, Expr A0, Expr A0) }
do_stmt
  : nonlabel_do_stmt newline        { $1 }

nonlabel_do_stmt :: { (VarName A0, Expr A0, Expr A0, Expr A0) }
nonlabel_do_stmt
  : DO loop_control                  { $2 }

loop_control :: { (VarName A0, Expr A0, Expr A0, Expr A0) }
loop_control
  : do_variable '=' int_expr ','  int_expr loop_control2  { ($1,$3,$5,$6) }
--  | int_expr comma_int_expr_opt comma_opt WHILE '(' scalar_logical_expr ')'

loop_control2 :: { Expr A0 }
loop_control2
  : ',' int_expr                                  { $2 }
  | {- empty -}                                   {% srcSpanNull >>= (\s -> return $ Con s "1") }


--comma_int_expr_opt :: { FExpr A0 }
--comma_int_expr_opt
--  : ',' int_expr                                  {  }
--  | {}                                            {  }

--comma_opt
--  : ','
--  | {}

do_block :: { Fortran A0 }
do_block
  : block                                         { $1 }

end_do :: { }
end_do
  : END DO {} 
  | ENDDO  {} 

block :: { Fortran A0 }
block
  : executable_construct_list                       { $1 }
  | {- empty -}                                     {% srcSpanNull >>= (return . NullStmt) }
 
execution_part :: { Fortran A0 }
execution_part 
  : executable_construct_list        { $1 }
| {- empty -}                        {% srcSpanNull >>= (return . NullStmt) }

executable_construct_list :: { Fortran A0 }
executable_construct_list
: executable_construct_list executable_construct_list  {% srcSpanFromL $1 (\s -> FSeq s $1 $2) }
| executable_construct  newline                        { $1 }

executable_construct :: { Fortran A0 }
executable_construct
  : NUM executable_construct                      {% (srcSpan l) >>= (\s -> return $ Label s $1 $2) }
--  | case_construct
  | do_construct                                  { $1 }
  | if_construct                                  { $1 }
  | action_stmt                                   { $1 }
--  | forall_construct
--  | where_construct
 

equivalence_stmt :: { Fortran A0 }
equivalence_stmt 
  : EQUIVALENCE '(' vlist ')'                     {% srcSpanFromL $1 (\s -> Equivalence s $3) }

action_stmt :: { Fortran A0 }
action_stmt
  : allocate_stmt                                 { $1 }
 | assignment_stmt                                { $1 }
  | backspace_stmt                                { $1 }
  | call_stmt                                     { $1 }
  | close_stmt                                    { $1 }
  | continue_stmt                                 { $1 }
  | cycle_stmt                                    { $1 }
  | deallocate_stmt                               { $1 }
  | endfile_stmt                                  { $1 }
  | equivalence_stmt                              { $1 }
--  | end_function_stmt
--  | end_program_stmt
--  | end_subroutine_stmt
  | exit_stmt                                     { $1 }
  | forall_stmt                                   { $1 }
  | goto_stmt                                     { $1 }
  | if_stmt                                       { $1 }
  | inquire_stmt                                  { $1 }
  | nullify_stmt                                  { $1 }
  | open_stmt                                     { $1 }
  | pointer_assignment_stmt                       { $1 }
  | print_stmt                                    { $1 }
  | read_stmt                                     { $1 }
  | return_stmt                                   { $1 }
  | rewind_stmt                                   { $1 }
  | stop_stmt                                     { $1 }
  | where_stmt                                    { $1 }
  | write_stmt                                    { $1 }
  | TEXT				          {% srcSpan l >>= (\s -> return $ TextStmt s $1) }

call_stmt :: { Fortran A0 }
call_stmt
  : CALL call_name '(' actual_arg_spec_list ')'   {% do { s1 <- srcSpanFrom $1;
                                                          s2 <- srcSpanFrom $4;
                                                          return $ Call s1 $2 (ArgList s2 $4); } }
  | CALL call_name '(' ')'                        {% do { s1 <- srcSpanFrom $1;
                                                          s2 <- srcSpanFrom $4;
                                                          return $ Call s1 $2 (ArgList s2 (NullExpr s2)); } }
  | CALL call_name                                {% do { s1 <- srcSpanFrom $1;
                                                          s2 <- srcSpanNull;
                                                          return $ Call s1 $2 (ArgList s2 (NullExpr s2)); } }

call_name :: { Expr A0 }
call_name
  : ID                 {% (srcSpan l) >>= (\s -> return $ Var s [(VarName s $1,[])]) }  

actual_arg_spec_list :: { Expr A0 }
actual_arg_spec_list
  : actual_arg_spec_list ',' actual_arg_spec      {% srcSpanFromL $1 (\s -> ESeq s $1 $3) }
  | actual_arg_spec                               { $1 }

actual_arg_spec :: { Expr A0 }
actual_arg_spec
  : ID '=' actual_arg                          {% srcSpan l >>= (\s -> return $ AssgExpr s $1 $3) }
  | actual_arg                                 { $1 }

actual_arg  :: { Expr A0 }
actual_arg
  : expr                                        { $1 }
--  | variable
--  | procedre_name
--  | alt_return_spec

else_if_list :: { [(Expr A0, Fortran A0)]  }
else_if_list
  : else_if_list else_if_then_stmt block   { $1++[($2,$3)] }
  | {- empty -}                            { [] }

else_if_stmt :: { Expr A0 }
else_if_stmt
  : ELSE if_then_stmt             { $2 }

if_then_stmt :: { Expr A0 }
if_then_stmt 
  : IF '(' logical_expr ')' THEN newline             { $3 }

else_if_then_stmt :: { Expr A0 }
else_if_then_stmt 
  : ELSEIF '(' logical_expr ')' THEN newline         { $3 }


--if_rest :: { ([(Expr A0,Fortran)],Maybe Fortran) }
--: ELSE if_then_stmt block if_rest     { (($2,$3):(fst $4),snd $4) }
--| ELSE block END IF                   { ([],Just $2) }
--| END IF                              { ([],Nothing) }

if_construct :: { Fortran A0 }
if_construct
 : if_then_stmt block end_if_stmt                  {% srcSpanFromL $1 (\s -> If s $1 $2 [] Nothing) }

--| if_then_stmt block ELSE block end_if_stmt      {% srcSpanFromL $1 (\s -> If s $1 $2 [] (Just $4)) }

| if_then_stmt block else_if_list end_if_stmt      {% srcSpanFromL $1 (\s -> If s $1 $2 $3 Nothing) }
| if_then_stmt block else_if_list ELSE newline block end_if_stmt    
                                                   {% srcSpanFromL $1 (\s -> If s $1 $2 $3 (Just $6)) }


--: if_then_stmt block if_rest				  { (If $1 $2 (fst $3) (snd $3)) }
--: if_then_stmt block else_if_list END IF                { (If $1 $2 $3 Nothing) }
--| if_then_stmt block else_if_list ELSE block END IF     { (If $1 $2 $3 (Just $5)) }
--| if_then_stmt block END IF                             { (If $1 $2 [] Nothing) }
--| if_then_stmt block ELSE block END IF                  { (If $1 $2 [] (Just $4)) }

--  : if_then_stmt block 
----    else_if_list 
--    else_opt 
--    END IF                                        { (If $1 $2 $3) }

end_if_stmt  :: {}
end_if_stmt  : END IF  { }
             | ENDIF   { } 


logical_expr :: { Expr A0 }
logical_expr
  : expr                                          { $1 }

allocate_stmt :: { Fortran A0 }
allocate_stmt
  : ALLOCATE '(' allocation_list ',' STAT '=' variable ')'    {% srcSpanFromL $1 (\s -> Allocate s $3 $7) }
      | ALLOCATE '(' allocation_list ')'                      {% srcSpanFromL $1 (\s -> Allocate s $3 (NullExpr s)) }
allocation_list :: { Expr A0 }
allocation_list
  : allocation_list ',' allocation                    {% srcSpanFromL $1 (\s -> ESeq s $1 $3) }
  | allocation                                        { $1 }
  | {- empty -}                                       {% srcSpanNull >>= (return . NullExpr) }

allocate_object_list :: { [Expr A0] }
allocate_object_list
  : allocate_object_list ',' allocate_object      { $1++[$3] }
  | allocate_object                               { [$1] }
allocate_object :: { Expr A0 }
allocate_object
  : scalar_variable_name_list                           {% srcSpanFromL (fst . head $ $1) (\s -> Var s $1) }

allocate_shape_spec_list :: { [Expr A0] }
allocate_shape_spec_list
  : allocate_shape_spec_list ',' allocate_shape_spec    { $1++[$3] }
  | allocate_shape_spec                                 { [$1] }
allocate_shape_spec :: { Expr A0 }
allocate_shape_spec
  : expr   { $1 }
  | bound  { $1 }
allocation :: { Expr A0 }
allocation
  : allocation_var_list2                          { $1 }

allocation_var_list2 :: { Expr A0 }
allocation_var_list2
  : allocation_var_list                          {% srcSpanFromL (fst . head $ $1) (\s -> Var s $1) }

allocation_var_list :: { [(VarName A0,[Expr A0])] }
allocation_var_list
  : allocation_var_list '%' allocation_var      { $1++[$3]  }
  | allocation_var                              { [$1] }

allocation_var :: { (VarName A0, [Expr A0]) }
allocation_var
  : ID '(' allocate_shape_spec_list ')'         {% srcSpan l >>= (\s -> return (VarName s $1, $3)) }
  | ID                                          {% srcSpan l >>= (\s -> return (VarName s $1, [])) }

backspace_stmt :: { Fortran A0 }
backspace_stmt
  : BACKSPACE expr                                {% srcSpanFromL $1 (\s -> Backspace s [NoSpec s $2]) }
  | BACKSPACE '(' position_spec_list ')'          {% srcSpanFromL $1 (\s -> Backspace s $3) }
position_spec_list :: { [Spec A0] }
position_spec_list
  : position_spec_list ',' position_spec          { $1++[$3] }
  | position_spec                                 { [$1] }
position_spec :: { Spec A0 }
position_spec
  : expr                                          {% srcSpanFromL $1 (\s -> NoSpec s $1) }
  | ID '=' expr                                   {% (srcSpan l) >>= (\s -> 
                                                      case (map (toLower) $1) of
                                                        "unit"   -> return (Unit   s $3)
                                                        "iostat" -> return (IOStat s $3)
                                                        s        ->  parseError ("incorrect name in spec list: " ++ s)) }
close_stmt :: { Fortran A0 }
close_stmt
  : CLOSE '(' close_spec_list ')'                 {% srcSpanFromL $1 (\s -> Close s $3) }
close_spec_list :: { [Spec A0] }
close_spec_list
  : close_spec_list ',' close_spec                { $1++[$3] }
  | close_spec                                    { [$1] }
close_spec :: { Spec A0 }
close_spec
  : expr                                          {% srcSpanFromL $1 (\s -> NoSpec s $1) }
  | ID '=' expr                                   {% (srcSpan l) >>= (\s ->
                                                      case (map (toLower) $1) of
                                                        "unit"   -> return (Unit   s $3)
                                                        "iostat" -> return (IOStat s $3)
                                                        "status" -> return (Status s $3)
                                                        s        -> parseError ("incorrect name in spec list: " ++ s)) }
--external_file_unit :: { Expr A0 }
--external_file_unit
--  : expr                                          { $1 }

continue_stmt :: { Fortran A0 }
continue_stmt
  : CONTINUE                                      {% srcSpanFromL $1 Continue }

cycle_stmt :: { Fortran A0 }
cycle_stmt
  : CYCLE id2                                      {% srcSpanFromL $1 (\s -> Cycle s $2) }
  | CYCLE                                          {% srcSpanFromL $1 (\s -> Cycle s "") }

deallocate_stmt :: { Fortran A0 }
deallocate_stmt
: DEALLOCATE '(' allocate_object_list ',' STAT '=' variable ')' 
                                                   {% srcSpanFromL $1 (\s -> Deallocate s $3 $7) }
| DEALLOCATE '(' allocate_object_list ')'          {% srcSpanFromL $1 (\s -> Deallocate s $3 (NullExpr s)) }

endfile_stmt :: { Fortran A0 }
endfile_stmt
  : ENDFILE expr                                  {% do { s1 <- srcSpanFrom $1;
                                                          s2 <- srcSpanFrom $2;
                                                          return $ Endfile s1 [NoSpec s2 $2]; } }
  | ENDFILE '(' position_spec_list ')'            {% srcSpanFromL $1 (\s -> Endfile s $3) }

exit_stmt :: { Fortran A0 }
exit_stmt
  : EXIT id2                                      {% srcSpanFromL $1 (\s -> Exit s $2) }
  | EXIT                                          {% srcSpanFromL $1 (\s -> Exit s "") }

forall_stmt :: { Fortran A0 }
forall_stmt 
  : FORALL forall_header forall_assignment_stmt      {% srcSpanFromL $1 (\s -> Forall s $2 $3) }
  | FORALL forall_header newline forall_assignment_stmt_list
                                forall_stmt_end      {% srcSpanFromL $1 (\s -> Forall s $2 $4) }

forall_stmt_end :: {}
forall_stmt_end 
  : END FORALL       {}
 | {- empty -}       {}

forall_header :: { ([(String,Expr A0,Expr A0,Expr A0)],Expr A0) }
forall_header
  : '(' forall_triplet_spec_list ',' expr ')'     { ($2,$4) }
  | '(' forall_triplet_spec_list ')'              {% srcSpanNull >>= (\s -> return ($2, NullExpr s)) }

forall_triplet_spec_list :: { [(String,Expr A0,Expr A0,Expr A0)] }
forall_triplet_spec_list
  : forall_triplet_spec_list ',' forall_triplet_spec  { $1++[$3]}
  | forall_triplet_spec                               { [$1] }

forall_triplet_spec :: { (String,Expr A0,Expr A0,Expr A0) }
forall_triplet_spec
  : id2 '=' int_expr ':' int_expr ';' int_expr { ($1,$3,$5,$7) }
  | id2 '=' int_expr ':' int_expr              {% srcSpanNull >>= (\s -> return ($1,$3,$5,NullExpr s)) }

forall_assignment_stmt :: { Fortran A0 }
forall_assignment_stmt
  : assignment_stmt                               { $1 }
  | pointer_assignment_stmt                       { $1 }

forall_assignment_stmt_l :: { Fortran A0 }
forall_assignment_stmt_l
  : forall_assignment_stmt newline { $1 }

forall_assignment_stmt_list :: { Fortran A0 }
forall_assignment_stmt_list 
  : forall_assignment_stmt_l forall_assignment_stmt_list {% srcSpanFromL $1 (\s -> FSeq s $1 $2) }
  | forall_assignment_stmt_l                             { $1 }


goto_stmt :: { Fortran A0 }
goto_stmt
  : GOTO NUM                                      {% srcSpanFromL $1 (\s -> Goto s $2) }

if_stmt :: { Fortran A0 }
if_stmt
  : IF '(' logical_expr ')' action_stmt           {% srcSpanFromL $1 (\s -> If s $3 $5 [] Nothing) }

inquire_stmt :: { Fortran A0 }
inquire_stmt
  : INQUIRE '(' inquire_spec_list ')'                       {% srcSpanFromL $1 (\s -> Inquire s $3 []) } 
  | INQUIRE '(' IOLENGTH '=' variable ')' output_item_list  {% do { s1 <- srcSpanFrom $1;
                                                                    s2 <- srcSpanFrom $5;
                                                                    return $ Inquire s1 [IOLength s2 $5] $7; } }
inquire_spec_list :: { [Spec A0] }
inquire_spec_list
  : inquire_spec_list ',' inquire_spec           { $1++[$3] }
  | inquire_spec                                 { [$1] }

inquire_spec :: { Spec A0 }
inquire_spec
  : expr                             {% srcSpanFromL $1 (\s -> NoSpec s $1) }
  | READ '=' variable                {% srcSpanFromL $1 (\s -> Read s $3) }
  | WRITE '=' variable               {% srcSpanFromL $1 (\s -> WriteSp s $3) }
  | ID '=' expr                      {% (srcSpan l) >>= (\s -> 
                                          case (map (toLower) $1) of
                                            "unit"        -> return (Unit s	  $3)
                                            "file"        -> return (File s	  $3)
                                            "iostat"      -> return (IOStat s     $3)
                                            "exist"       -> return (Exist s      $3)
                                            "opened"      -> return (Opened s     $3)
                                            "number"      -> return (Number s     $3)
                                            "named"       -> return (Named s      $3)
                                            "name"        -> return (Name s       $3)
                                            "access"      -> return (Access s     $3)
                                            "sequential"  -> return (Sequential s $3)
                                            "direct"      -> return (Direct s     $3)
                                            "form"        -> return (Form s       $3)
                                            "formatted"   -> return (Formatted s  $3)
                                            "unformatted" -> return (Unformatted s $3)
                                            "recl"        -> return (Recl    s   $3)
                                            "nextrec"     -> return (NextRec s   $3)
                                            "blank"       -> return (Blank   s   $3)
                                            "position"    -> return (Position s  $3)
                                            "action"      -> return (Action   s  $3)
                                            "readwrite"   -> return (ReadWrite s $3)
                                            "delim"       -> return (Delim    s  $3)
                                            "pad"         -> return (Pad     s   $3)
                                            s             -> parseError ("incorrect name in spec list: " ++ s))}
--io_implied_do
--io_implied_do
--  : '(' io_implied_do_object_list ',' io_implied_do_control ')'
--io_implied_do_object
--io_implied_do_object
--  : input_item
--  | output_item
--io_implied_do_control
--io_implied_do_control
--  : do_variable '=' scalar_int_expr ',' scalar_int_expr ',' scalar_int_expr
--  | do_variable '=' scalar_int_expr ',' scalar_int_expr
--file_name_expr
--file_name_expr
--  : scalar_char_expr



nullify_stmt :: { Fortran A0 }
nullify_stmt
  : NULLIFY '(' pointer_object_list ')'           {% srcSpanFromL $1 (\s -> Nullify s $3) }

pointer_object_list :: { [Expr A0] }
pointer_object_list
  : pointer_object_list ',' pointer_object        { $1++[$3] }
  | pointer_object                                { [$1] }

pointer_object :: { Expr A0 }
pointer_object
--  : ID                                            { (Var [VarName $1] []) }
  : structure_component                           { $1 }

structure_component :: { Expr A0 }
structure_component
  : part_ref                                      { $1 }

open_stmt :: { Fortran A0 }
open_stmt
  : OPEN '(' connect_spec_list ')'                {% srcSpanFromL $1 (\s -> Open s $3) }

connect_spec_list :: { [Spec A0] }
connect_spec_list
  : connect_spec_list ',' connect_spec            { $1++[$3] }
  | connect_spec                                  { [$1] }

connect_spec :: { Spec A0 }
connect_spec
  : expr                           {% srcSpanFromL $1 (\s -> NoSpec s $1) }
  | ID '=' expr                    {% (srcSpan l) >>= (\s ->
                                        case (map (toLower) $1) of
                                          "unit"     -> return (Unit s $3)  
                                          "iostat"   -> return (IOStat s $3)
                                          "file"     -> return (File s $3)
                                          "status"   -> return (Status s $3)
                                          "access"   -> return (Access s $3)
                                          "form"     -> return (Form s $3)
                                          "recl"     -> return (Recl s $3)
                                          "blank"    -> return (Blank s $3)
                                          "position" -> return (Position s $3)
                                          "action"   -> return (Action s $3)
                                          "delim"    -> return (Delim s $3)
                                          "pad"      -> return (Pad s $3)
                                          s          -> parseError ("incorrect name in spec list: " ++ s)) }
file_name_expr :: { Expr A0 }
file_name_expr
  : scalar_char_expr                              { $1 }

scalar_char_expr :: { Expr A0 }
scalar_char_expr
  : expr                                          { $1 }

scalar_int_expr :: { Expr A0 }
scalar_int_expr
  : expr                                          { $1 }

pointer_assignment_stmt :: { Fortran A0 }
pointer_assignment_stmt
  : pointer_object '=>' target                    {% srcSpanFromL $1 (\s -> PointerAssg s $1 $3) }

target :: { Expr A0 }
target
  : expr                                          { $1 }



print_stmt :: { Fortran A0 }
print_stmt
  : PRINT format ',' output_item_list           {% srcSpanFromL $1 (\s -> Print s $2 $4) }
  | PRINT format                                {% srcSpanFromL $1 (\s -> Print s $2 []) }

-- also replaces io_unit
format :: { Expr A0 }
format
  : expr                                          { $1 }
--  | literal_constant                              { (Con $1) } -- label
  | '*'                                           {% srcSpanFromL $1 (\s -> Var s [(VarName s "*",[])]) }

output_item_list :: { [Expr A0] }
output_item_list
  : output_item_list ','  output_item             { $1++[$3] }
  | output_item                                   { [$1] }

output_item :: { Expr A0 }
output_item
  : expr                                          { $1 }
--  | io_implied_do                                 { $1 }


read_stmt :: { Fortran A0 }
read_stmt
  : READ '(' io_control_spec_list ')' input_item_list {% srcSpanFromL $1 (\s -> ReadS s $3 $5) }
  | READ '(' io_control_spec_list ')'                 {% srcSpanFromL $1 (\s -> ReadS s $3 []) }
--  | READ format ',' output_item_list                  { (ReadS [NoSpec $2] $4) }
--  | READ format                                       { (ReadS [NoSpec $2] []) }

io_control_spec_list :: { [Spec A0] }
io_control_spec_list
  : io_control_spec_list ',' io_control_spec      { $1++[$3] }
  | io_control_spec                               { [$1] }
-- (unit, fmt = format), (rec, advance = expr), (nml, iostat, id = var), (err, end, eor = label)
io_control_spec :: { Spec A0 } 
io_control_spec
  : format                                        {% srcSpanFromL $1 (\s -> NoSpec s $1) }
  | END '=' label                                 {% srcSpanFromL $1 (\s -> End s $3) }
  | ID '=' format                                 {% (srcSpan l) >>= (\s ->
                                                     case (map (toLower) $1) of
                                                     "unit"    -> return (Unit s $3)
                                                     "fmt"     -> return (FMT s $3)
                                                     "rec"     -> return (Rec s $3)
                                                     "advance" -> return (Advance s $3)
                                                     "nml"     -> return (NML s $3)
                                                     "iostat"  -> return (IOStat s $3)
                                                     "size"    -> return (Size s $3)
                                                     "eor"     -> return (Eor s $3)
                                                     s         -> parseError ("incorrect name in spec list: " ++ s)) }
--  | namelist_group_name                           { NoSpec $1 }
input_item_list :: { [Expr A0] }
input_item_list
  : input_item_list ',' input_item                { $1++[$3] }
  | input_item                                    { [$1] }
input_item :: { Expr A0 }
input_item
  : variable                                      { $1 }
--  | io_implied_do
--io_unit :: { Expr A0 }
--io_unit
--  : expr                                          { $1 }
--  | '*'                                           { (Var [(VarName "*",[])]) }
--  | internal_file_unit                            { $1 }

label :: { Expr A0 }
label
  : NUM                                           {% (srcSpan l) >>= (\s -> return $ Con s $1) }

--internal_file_unit :: { Expr A0 }
--internal_file_unit
--  : default_char_variable                         { $1 }

--default_char_variable :: { Expr A0 }
--default_char_variable
--  : variable                                      { $1 }
namelist_group_name :: { Expr A0 }
namelist_group_name
  : variable                                      { $1 }


return_stmt :: { Fortran A0 }
return_stmt
  : RETURN                                        {% srcSpanFromL $1 (\s -> Return s (NullExpr s)) }
  | RETURN int_expr                               {% srcSpanFromL $1 (\s -> Return s $2) }

scalar_default_int_variable :: { Expr A0 }
scalar_default_int_variable
  : variable                                      { $1 }

scalar_default_char_expr :: { Expr A0 }
scalar_default_char_expr
  : expr                                          { $1 }

rewind_stmt :: { Fortran A0 }
rewind_stmt
  : REWIND expr                                  {% do { s1 <- srcSpanFrom $1;
                                                         s2 <- srcSpanNull;
                                                         return $ Rewind s1 [NoSpec s2 $2]; } }
  | REWIND '(' position_spec_list ')'            {% srcSpanFromL $1 (\s ->Rewind s $3) }



stop_stmt :: { Fortran A0 }
stop_stmt
  : STOP stop_code                               {% srcSpanFromL $1 (\s -> Stop s $2) }
  | STOP                                         {% srcSpanFromL $1 (\s -> Stop s (NullExpr s)) }

stop_code :: { Expr A0 }
stop_code
  : constant                                     { $1 }
  


where_stmt :: { Fortran A0 }
where_stmt
  : WHERE '(' mask_expr ')' where_assignment_stmt {% srcSpanFromL $1 (\s -> Where s $3 $5) }

where_assignment_stmt :: { Fortran A0 }
where_assignment_stmt
  : assignment_stmt                              { $1 }
mask_expr :: { Expr A0 }
mask_expr
  : logical_expr                                 { $1 }



write_stmt :: { Fortran A0 }
write_stmt
  : WRITE '(' io_control_spec_list ')' output_item_list  {% srcSpanFromL $1 (\s -> Write s $3 $5) }
  | WRITE '(' io_control_spec_list ')'                   {% srcSpanFromL $1 (\s -> Write s $3 []) }


{

-- Initial annotations from parser

-- Type of annotations

type A0 = (SrcLoc, SrcLoc) 

{- Given a source location (usually token start),
get the current src loc from the parser monad (usually token end), 
return as pair giving bounds on the syntax span -}

srcSpan :: SrcLoc -> P A0
srcSpan l = do l' <- getSrcLoc
               return $ (l, l')

-- 0-length span at current position

srcSpanNull :: P A0
srcSpanNull = do l <- getSrcLoc
                 return $ (l, l)

-- Combinators to generate spans anchored at existing elements

class SrcSpanFromAnnotation t where
   srcSpanFrom :: Copointed d => d t -> P A0

   srcSpanFromL :: Copointed d => d t -> (A0 -> b) -> P b
   srcSpanFromL x f = do a <- srcSpanFrom x
                         return $ f a

instance SrcSpanFromAnnotation A0 where
   srcSpanFrom x = do let l = fst $ copoint x
                      l' <- getSrcLoc
                      return $ (l, l')

instance SrcSpanFromAnnotation SrcLoc where
   srcSpanFrom x = do let l = copoint x
                      l' <- getSrcLoc
                      return $ (l, l')

happyError :: P a
happyError = parseError "syntax error"

parseError :: String -> P a
parseError m = do srcloc <- getSrcLoc 
		  fail ("line " ++ show (srcLine srcloc) ++ " column " ++ show (srcColumn srcloc) ++ ": " ++ m ++ "\n")

tokenFollows s = case alexScan ('\0',s) 0 of
                    AlexEOF               -> "end of file"
                    AlexError  _          -> ""
                    AlexSkip  (_,t) len   -> tokenFollows t
                    AlexToken (_,t) len _ -> take len s

parse :: String -> [Program A0]
parse p = case (runParser parser p) of 
	    (ParseOk p)       -> p
            (ParseFailed l e) ->  error e

--parse :: String -> [Program]
--parse = clean . parser . fixdecls . scan

parseF :: String -> IO ()
parseF f = do s <- readFile f
              print (parse s)

--scanF :: String -> IO ()
--scanF f = do s <- readFile f
--             print (scan s)

fst3 (a,b,c) = a
snd3 (a,b,c) = b
trd3 (a,b,c) = c

fst4 (a,b,c,d) = a
snd4 (a,b,c,d) = b
trd4 (a,b,c,d) = c
frh4 (a,b,c,d) = d

cmpNames :: SubName A0 -> String -> String -> P (SubName A0)
cmpNames x "" z                        = return x
cmpNames (SubName a x) y z | x==y      = return (SubName a x)
                           | otherwise = parseError (z ++ " name \""++x++"\" does not match \""++y++"\" in end " ++ z ++ " statement\n")
cmpNames s y z                       = parseError (z ++" names do not match\n")
					   
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

expr2array_spec (Bound a e e') = (e, e') -- possibly a bit dodgy- uses undefined
expr2array_spec e = (NullExpr undefined, e)

}
