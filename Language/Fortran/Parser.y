{
 {-# LANGUAGE QuasiQuotes #-}
 {-# LANGUAGE TypeSynonymInstances #-}
 {-# LANGUAGE FlexibleInstances #-}

module Language.Fortran.Parser  where

import Language.Fortran

import Language.Haskell.Syntax (SrcLoc(..))
import Language.Haskell.ParseMonad 
import Language.Fortran.Lexer
import Data.Char (toLower)

}

%name parser
%tokentype { Token }

%monad { P } { >>= } { return }
%lexer { lexer } { TokEOF }

%token
 '=>'			{ Arrow }
 '**'			{ OpPower }
 '//' 			{ OpConcat }
 '=='		        { OpEQ }
 '/='       		{ OpNE }
 '<='		        { OpLE }
 '>='		        { OpGE }
 '.NOT.'		{ OpNOT }
 '.AND.'		{ OpAND }
 '.OR.'		        { OpOR }
 '.TRUE.'		{ TrueConst }
 '.FALSE.'		{ FalseConst }
-- '.EQV.'		{ OpEQV }
-- '.NEGV.' 	       	{ OpNEQV }
 '<'		        { OpLT }
 '>'		        { OpGT }
 '*'		       	{ OpMul }
 '/'		       	{ OpDiv }
 '+'		       	{ OpAdd }
 '-'		       	{ OpSub }
 ','		       	{ Comma }
 '('		       	{ LParen }
 ')'		       	{ RParen }
 '='		       	{ OpEquals }
-- '\''		      	{ SingleQuote }
-- '\"'			{ DoubleQuote }
 '.'		        { Period }
 '::'			{ ColonColon }
 ':'			{ Colon }
 ';'                    { SemiColon }
 '#'                    { Hash }
 '{'                    { LBrace }
 '}'                    { RBrace }
 '(/'                   { LArrCon }
 '/)'                   { RArrCon }
--'b'                    { LitMark $$ }
--'B'                    { LitMark $$ }
--'z'                    { LitMark $$ }
--'Z'                    { LitMark $$ }
--'o'                    { LitMark $$ }
--'O'                    { LitMark $$ }
-- OBSOLETE '!'         { Bang } 
 '%'			{ Percent }
 '$'			{ Dollar }
 -- OBSOLETE '!{'	{ StopParamStart }
'\n'                    { NewLine }
 ALLOCATE 		{ Key "allocate" }
 ALLOCATABLE 		{ Key "allocatable" }
 ASSIGN 		{ Key "Assign" }
 ASSIGNMENT 		{ Key "assignment" }
-- AUTOMATIC 		{ Key "automatic" }
 BACKSPACE 		{ Key "backspace" }
 BLOCK 			{ Key "block" }
 CALL 			{ Key "call" }
-- CASE 		{ Key "case" }
 CHARACTER 		{ Key "character" }
 CLOSE 			{ Key "close" }
 COMMON 		{ Key "common" }
 COMPLEX 		{ Key "complex" }
 CONTAINS 		{ Key "contains" }
 CONTINUE 		{ Key "continue" }
 CYCLE 			{ Key "cycle" }
 DATA 			{ Key "data" }
 DEALLOCATE 		{ Key "deallocate" }
-- DEFAULT 		{ Key "default" }
 DIMENSION 		{ Key "dimension" }
 DO 			{ Key "do" }
-- DOUBLE 		{ Key "double" }
 ELEMENTAL 		{ Key "elemental" }
 ELSE 			{ Key "else" }
 ELSEIF 		{ Key "elseif" }
-- ELSEWHERE 		{ Key "elsewhere" }
 END 			{ Key "end" }
 ENDIF			{ Key "endif" }
 ENDDO			{ Key "enddo" }
 ENDFILE                { Key "endfile" }
-- ENTRY 		{ Key "entry" }
 EQUIVALENCE 		{ Key "equivalence" }
 EXIT 			{ Key "exit" }
 EXTERNAL 		{ Key "external" }
 FORALL 		{ Key "forall" }
 FOREACH		{ Key "foreach" }
-- FORMAT 		{ Key "format" }
 FUNCTION 		{ Key "function" }
 GOTO 			{ Key "goto" }
 IOLENGTH               { Key "iolength" }
 IF 			{ Key "if" }
 IMPLICIT 		{ Key "implicit" }
 IN 			{ Key "in" }
 INCLUDE		{ Key "include" }
 INOUT 			{ Key "inout" }
 INTEGER 		{ Key "integer" }
 INTENT 		{ Key "intent" }
 INTERFACE 		{ Key "interface" }
 INTRINSIC 		{ Key "intrinsic" }
 INQUIRE 		{ Key "inquire" }
 KIND 			{ Key "kind" }
 LEN 			{ Key "len" }
 LOGICAL 		{ Key "logical" }
 MODULE 		{ Key "module" }
 NAMELIST 		{ Key "namelist" }
 NONE 			{ Key "none" }
 NULLIFY 		{ Key "nullify" }
 NULL 			{ Key "null" }
-- ONLY 		{ Key "only" }
 OPEN 			{ Key "open" }
 OPERATOR 		{ Key "operator" }
 OPTIONAL 		{ Key "optional" }
 OUT 			{ Key "out" }
 PARAMETER 		{ Key "parameter" }
-- PAUSE 		{ Key "pause" }
 POINTER 		{ Key "pointer" }
-- PRECISION 		{ Key "precision" }
 PRINT 			{ Key "print" }
 PRIVATE 		{ Key "private" }
 PROCEDURE 		{ Key "procedure" }
 PROGRAM 		{ Key "program" }
 PURE 			{ Key "pure" }
 PUBLIC 		{ Key "public" }
 REAL 			{ Key "real" }
 READ 			{ Key "read" }
 RECURSIVE 		{ Key "recursive" }
 RESULT 		{ Key "result" }
 RETURN 		{ Key "return" }
 REWIND 		{ Key "rewind" }
 SAVE 			{ Key "save" }
-- SELECT 		{ Key "select" }
 SEQUENCE 		{ Key "sequence" }
-- SIZE 		{ Key "size" }
 SOMETYPE               { Key "sometype" }
 SQRT			{ Key "sqrt" }
 STAT 			{ Key "stat" }
 STOP			{ Key "stop" }
 STR                    { StrConst $$ }
 ZLIT                   { LitConst 'z' $$ }
 SUBROUTINE 		{ Key "subroutine" }
 TARGET 		{ Key "target" }
-- TO 			{ Key "to" }
 THEN 			{ Key "then" }
 TYPE 			{ Key "type" }
-- UNFORMATED 		{ Key "unformatted" }
 UNIT 			{ Key "unit" }
 USE 			{ Key "use" }
 VOLATILE 		{ Key "volatile" }
 WHERE 			{ Key "where" }
 WRITE 			{ Key "write" }
 ID                     { ID $$ }
 '1'                    { Num "1" }
 NUM                    { Num $$ }
 TEXT                   { Text $$ }
%%

executable_program :: { Program A0 }
executable_program
  : program_unit_list                             { $1 }
    
program_unit_list :: { Program A0 }
program_unit_list
  : program_unit_list newline0 program_unit       { $1++[$3] }
  | {- empty -}                                   { [] }

program_unit :: { ProgUnit A0 }
program_unit
  : main_program                                  { $1 }
  | external_subprogram                           { $1 }
  | module                                        { $1 }
  | block_data                                    { $1 }

plist :: { [String] }
plist 
  : plist ',' id2                                 { $1++[$3] }
  | id2                                           { [$1] }

vlist :: { [Expr A0] }
vlist 
  : variable ',' vlist                            { $1:$3 }
  | variable                                      { [$1] }

newline :: {}
newline : '\n' newline0 {}

newline0 :: {}
newline0 : newline  {} 
        | {- empty -} {}

main_program :: { ProgUnit A0 }
main_program
  : srcloc program_stmt use_stmt_list implicit_part srcloc specification_part_top execution_part module_subprogram_part end_program_stmt newline0
                {% do { s <- srcSpan $1;
		        s' <- srcSpan $5;
		        name <- cmpNames (fst $2) $9 "program";
		        return (Main () s name (snd $2) (Block () $3 $4 s' $6 $7) $8); } }

program_stmt :: { (SubName A0, Arg A0) }
program_stmt
  : PROGRAM subname args_p newline   { ($2, $3) }
  | PROGRAM subname srcloc newline   { ($2, (Arg () (NullArg ())) ($3, $3)) } 

end_program_stmt :: { String }
end_program_stmt
  : END PROGRAM id2   { $3 }
  | END PROGRAM       { "" }
  | END               { "" }

implicit_part :: { Implicit A0 }
implicit_part
  : IMPLICIT NONE newline   { ImplicitNone () }
  | {- empty -}             { ImplicitNull () }

external_subprogram :: { ProgUnit A0}
external_subprogram
  : function_subprogram           { $1 }
  | subroutine_subprogram         { $1 } 

subroutine_subprogram :: { ProgUnit A0 }
subroutine_subprogram 
  : srcloc subroutine_stmt use_stmt_list implicit_part srcloc specification_part_top execution_part end_subroutine_stmt newline0
  {% do { s <- srcSpan $1;
          s' <- srcSpan $5;
          name <- cmpNames (fst3 $2) $8 "subroutine";
          return (Sub () s (trd3 $2) name (snd3 $2) (Block () $3 $4 s' $6 $7)); } }

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

function_subprogram :: { ProgUnit A0 }
function_subprogram
: srcloc function_stmt use_stmt_list implicit_part srcloc specification_part_top execution_part end_function_stmt newline0  {% do { s <- srcSpan $1;
                       s' <- srcSpan $5;
                       name <- cmpNames (fst4 $2) $8 "function";
		       return (Function () s (trd4 $2) name (snd4 $2) (frh4 $2) (Block () $3 $4 s' $6 $7)); } }

block_data :: { ProgUnit A0 }
block_data
  : srcloc block_data_stmt use_stmt_list implicit_part specification_part_top end_block_data_stmt     
                  {% do { s <- srcSpan $1;
                          name <- cmpNames $2 $6 "block data";
                          return (BlockData () s name $3 $4 $5); } }
  
block_data_stmt :: { SubName A0 }
block_data_stmt
  : BLOCK DATA subname                     { $3 } 
| BLOCK DATA                               { NullSubName () } 

end_block_data_stmt :: { String }
end_block_data_stmt
  : END BLOCK DATA id2                     { $4 }
  | END BLOCK DATA                         { "" }
  | END                                    { "" }
  
module :: { ProgUnit A0 }
module
   : srcloc module_stmt use_stmt_list implicit_part specification_part_top module_subprogram_part end_module_stmt
         {%  do { s <- srcSpan $1;
                  name <- cmpNames $2 $7  "module";
		  return (Module ()s name $3 $4 $5 $6); } }

module_stmt :: { SubName A0 }
module_stmt
  : MODULE subname newline                    { $2 } 

end_module_stmt :: { String }
end_module_stmt
  : END MODULE id2                            { $3 }
  | END MODULE                                { "" }
  | END                                       { "" }

module_subprogram_part :: { Program A0 }
module_subprogram_part
  : CONTAINS newline internal_subprogram_list { $3 }
| {- empty -}                                 { [] } 
  
internal_subprogram_list :: { Program A0 }
internal_subprogram_list
  : internal_subprogram_list internal_subprogram newline0 { $1++[$2] } 
  | {- empty -}                                           { [] }
  
internal_subprogram :: { ProgUnit A0 }
internal_subprogram
  : subroutine_subprogram                           { $1 }
  | function_subprogram                             { $1 }
  
use_stmt_list :: { Uses A0 }
use_stmt_list
: use_stmt_list use_stmt  { Use () $2 $1 () }
| {- empty -}  	  { UseNil () }

use_stmt :: { (String, Renames) }
use_stmt
: USE id2 newline { ($2, []) }
| USE id2 ',' renames newline { ($2, $4) }

renames :: { [(Variable, Variable)] }
:  id2 '=>' id2        { [($1, $3)] }
 | renames ',' renames { $1 ++ $3 }
 
  
-- [DO: Allows the specification part of a module to be empty]
specification_part_top :: { Decl A0 }
specification_part_top
   : specification_part   { $1 }
| {- empty -}           {% srcSpanNull >>= (\s -> return $ NullDecl  () s)}

specification_part :: { Decl A0 }
specification_part
  : declaration_construct_l specification_part { DSeq () $1 $2 }
  | declaration_construct_l                    { $1 }
  

declaration_construct_l :: { Decl A0 }
declaration_construct_l
  : declaration_construct_p newline { $1 }

declaration_construct_p :: { Decl A0 }
declaration_construct_p
  : declaration_construct                         { $1 }
  | specification_stmt                            { $1 }
  | derived_type_def                              { $1 }
  | TEXT					  { TextDecl () $1 }

-- Not sure about the ArrayT outputs here, think this may be a bug

declaration_construct :: { Decl A0 }
declaration_construct
  : srcloc type_spec_p attr_spec_list '::' entity_decl_list  
  {% (srcSpan $1) >>= (\s -> return $ if isEmpty (fst $3) 
					 then Decl () s $5 ((BaseType () (fst3 $2) (snd $3) (snd3 $2) (trd3 $2)))
			                 else Decl () s $5 ((ArrayT ()  (fst $3) (fst3 $2) (snd $3) (snd3 $2) (trd3 $2)))) }
  | srcloc type_spec_p attr_spec_list entity_decl_list  
  {% (srcSpan $1) >>= (\s -> return $ if isEmpty (fst $3) 
					     then Decl () s $4 ((BaseType () (fst3 $2) (snd $3) (snd3 $2) (trd3 $2)))
			     	             else Decl () s $4 ((ArrayT () (fst $3) (fst3 $2) (snd $3) (snd3 $2) (trd3 $2)))) }
  | interface_block				      { $1 }
  | include_stmt { $1 }


attr_spec_list :: {([(Expr A0, Expr A0)],[Attr A0])}
attr_spec_list
  : attr_spec_list ',' attr_spec                  { (fst $1++fst $3,snd $1++snd $3) }
  | {- empty -}                                   { ([],[]) }

entity_decl_list :: { [(Expr A0, Expr A0)] }
entity_decl_list
: entity_decl ',' entity_decl_list         { $1:$3 }
| entity_decl                              { [$1] }

entity_decl :: { (Expr A0, Expr A0) }
entity_decl
  : srcloc ID '=' expr   {% srcSpan $1 >>= (\s -> return $ (Var () s [(VarName () $2,[])], $4)) }
  | variable             {% srcSpanNull >>= (\s -> return $ ($1, NullExpr () s)) }  

object_name :: { String }
object_name
  : id2                                            { $1 }

type_spec_p :: { (BaseType A0, Expr A0, Expr A0) }
type_spec_p
  : type_spec                                     { (fst3 $1, snd3 $1, trd3 $1) }

type_spec :: { (BaseType A0, Expr A0, Expr A0) }
type_spec
: INTEGER kind_selector                         {% srcSpanNull >>= (\s -> return $ (Integer (), $2, NullExpr () s))  }
| INTEGER '*' length_value                      {% srcSpanNull >>= (\s -> return $  (Integer (), $3, NullExpr () s)) }
| INTEGER                                       {% srcSpanNull >>= (\s -> return $  (Integer (), NullExpr () s, NullExpr () s)) }
| REAL kind_selector                            {% srcSpanNull >>= (\s -> return $  (Real (), $2, NullExpr () s)) }
| REAL '*' length_value                         {% srcSpanNull >>= (\s -> return $  (Real (), $3, NullExpr () s)) }
| REAL                                          {% srcSpanNull >>= (\s -> return $  (Real (), NullExpr () s, NullExpr () s)) }
| SOMETYPE                                      {% srcSpanNull >>= (\s -> return $  (SomeType (), NullExpr () s, NullExpr () s)) }
--  | DOUBLE PRECISION kind_selector                { (Double (), $3, ne s)) }
--  | DOUBLE PRECISION '*' length_value             { (Double (), $4, ne s)) }
--  | DOUBLE PRECISION                              { (Double (), ne s, ne s)) }
| COMPLEX kind_selector                         {% srcSpanNull >>= (\s -> return $  (Complex (), $2, NullExpr () s)) }
| COMPLEX '*' length_value                      {% srcSpanNull >>= (\s -> return $  (Complex (), $3, NullExpr () s)) }
| COMPLEX                                       {% srcSpanNull >>= (\s -> return $  (Complex (),NullExpr () s, NullExpr () s)) }
| CHARACTER char_selector                       { (Character (), snd $2, fst $2) }
| CHARACTER                                     {% srcSpanNull >>= (\s -> return $  (Character (), NullExpr () s, NullExpr () s)) }
| LOGICAL kind_selector                         {% srcSpanNull >>= (\s -> return $  (Logical (), $2, NullExpr () s)) }
| LOGICAL '*' length_value                      {% srcSpanNull >>= (\s -> return $  (Logical (), $3, NullExpr () s)) }
| LOGICAL                                       {% srcSpanNull >>= (\s -> return $  (Logical (), NullExpr () s, NullExpr () s)) }
| TYPE '(' type_name ')'                        {% srcSpanNull >>= (\s -> return $ (DerivedType () $3, NullExpr () s, NullExpr () s)) }

--  | POINTER '(' pointer_name ',' pointee_name ['(' array_spec ')' ] ')'
--[',' '(' pointer_name ',' pointee_name ['(' array_spec ')' ] ')' ] 

kind_selector :: { Expr A0 }
  : '(' KIND '=' expr ')'                         { $4 }
  | '(' expr ')'                                  { $2 }

char_selector :: { (Expr A0, Expr A0) }  -- (LEN, KIND)
char_selector 
: length_selector                                         {% srcSpanNull >>= (\s -> return $ ($1,NullExpr () s)) }
| '(' LEN '=' char_len_param_value ',' KIND '=' expr ')'  { ($4,$8) }
| '(' char_len_param_value ',' KIND '=' expr ')'          { ($2,$6) }
| '(' char_len_param_value ',' expr ')'                   {% srcSpanNull >>= (\s -> return $   ($2,NullExpr () s)) }
| '(' KIND '=' expr ',' LEN '=' char_len_param_value ')'  { ($8,$4) }
| '(' KIND '=' expr ')'                                   {% srcSpanNull >>= (\s -> return $   (NullExpr () s,$4)) }

length_selector :: { Expr A0 }
length_selector 
: '(' LEN '=' char_len_param_value ')'                    { $4 }
| '(' char_len_param_value ')'                            { $2 }

char_len_param_value :: { Expr A0 }
char_len_param_value
: specification_expr                                     { $1 }
| srcloc '*'                                             {% srcSpan $1 >>= (\s -> return $ Con () s "*") }

length_value :: { Expr A0 }
length_value
: srcloc num                                           {% srcSpan $1 >>= (\s -> return $ Con () s $2) }

dim_spec :: { [(Expr A0, Expr A0)] }
dim_spec
  : DIMENSION '(' array_spec ')' { $3 }
  | DIMENSION '(' ')'            { [] }  -- modified by Zhe on 11/14/2004

attr_spec :: { ([(Expr A0, Expr A0)],[Attr A0]) }
attr_spec
: dim_spec                                       { ($1,[]) }
| PARAMETER                                      { ([],[Parameter ()]) }
| access_spec                                    { ([],[$1]) }
| ALLOCATABLE                                    { ([],[Allocatable ()]) }
| EXTERNAL                                       { ([],[External ()]) }
| INTENT '(' intent_spec ')'                     { ([],[Intent () $3]) }
| INTRINSIC                                      { ([],[Intrinsic ()]) }
| OPTIONAL                                       { ([],[Optional ()]) }
| POINTER                                        { ([],[Pointer ()]) }
| SAVE                                           { ([],[Save ()]) }
| TARGET                                         { ([],[Target ()]) }
| UNIT '(' unit_spec ')'                         { ([],[MeasureUnit () $3]) }
| VOLATILE                                       { ([],[Volatile ()]) }

access_spec :: { Attr A0 }
access_spec
: PUBLIC            { Public () }
| PRIVATE           { Private () }

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
: INCLUDE srcloc STR       {% srcSpan $2 >>= (\s -> return $ Include () (Con () s $3)) }

specification_expr :: { Expr A0 }
specification_expr
  : expr { $1 } 

intent_spec :: { IntentAttr A0 }
intent_spec
: IN            {  In () }
| OUT           { Out () }
| INOUT         { InOut () }

unit_spec :: { MeasureUnitSpec A0 }
unit_spec
: mult_unit_spec '/' mult_unit_spec { UnitQuotient () $1 $3 }
| mult_unit_spec                    { UnitProduct () $1 }
| {- empty -}                       { UnitNone () }

mult_unit_spec :: { [(MeasureUnit, Fraction A0)] }
mult_unit_spec
: mult_unit_spec power_unit_spec { $1++$2 }
| power_unit_spec                { $1 }

power_unit_spec :: { [(MeasureUnit, Fraction A0)] }
power_unit_spec
: ID '**' power_spec { [($1, $3)] }
| ID                 { [($1, NullFraction ())] }
| '1'                { [] }

power_spec :: { Fraction A0 }
power_spec
: '(' signed_num '/' signed_num ')' { FractionConst () $2 $4 }
| signed_num                        { IntegerConst () $1 }
| '(' power_spec ')'                { $2 }

signed_num :: { String }
signed_num
: '-' num { "-" ++ $2 }
| num     { $1 }

specification_stmt :: { Decl A0 }
specification_stmt
  : access_stmt            { $1 }
--  | allocatable_stmt       { $1 }
  | common_stmt            { $1 }
  | data_stmt              { $1 }
  | equivalence_stmt              { $1 }
--  | dimension_stmt         { $1 }
  | external_stmt          { $1 }
--  | intent_stmt            { $1 }
--  | intrinsic_stmt         { $1 }
  | namelist_stmt            { $1 }
--  | optional_stmt          { $1 }
--  | pointer_stmt           { $1 }
  | save_stmt              { $1 }
--  | target_stmt            { $1 }
  | unit_stmt              { $1 }

save_stmt :: { Decl A0 }
 : SAVE { AccessStmt () (Save ()) [] }

common_stmt :: { Decl A0 }
: srcloc COMMON '/' id2 '/' vlist  {% srcSpan $1 >>= (\s -> return $ Common () s (Just $4) $6) }
| srcloc COMMON vlist              {% srcSpan $1 >>= (\s -> return $ Common () s Nothing $3) }


interface_block :: { Decl A0 }
interface_block
: interface_stmt newline interface_spec_list newline end_interface_stmt  { Interface () $1 $3 }

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
        {% do { name <- cmpNames (fst4 $1) $5 "interface declaration";
	        return (FunctionInterface ()  name (snd4 $1) $2 $3 $4); }}

  | function_stmt end_function_stmt  
        {% do { name <- cmpNames (fst4 $1) $2 "interface declaration";
	        s <- srcSpanNull;
	        return (FunctionInterface () name (snd4 $1) (UseNil ()) (ImplicitNull ()) (NullDecl () s)); } }

  | subroutine_stmt use_stmt_list implicit_part specification_part end_subroutine_stmt
        {% do { name <- cmpNames (fst3 $1) $5 "interface declaration";
                return (SubroutineInterface () name (snd3 $1) $2 $3 $4); } }

  | subroutine_stmt end_subroutine_stmt 
        {% do { name <- cmpNames (fst3 $1) $2 "interface declaration";
	        s <- srcSpanNull;
	        return (SubroutineInterface () name (snd3 $1) (UseNil ()) (ImplicitNull ()) (NullDecl () s)); }}
  
module_procedure_stmt :: { InterfaceSpec A0 }
module_procedure_stmt
: MODULE PROCEDURE sub_name_list    { ModuleProcedure () $3 }

sub_name_list :: { [SubName A0 ] }
sub_name_list
  :  sub_name_list ',' sub_name  { $1++[$3] }
  |  sub_name                    { [$1] }

sub_name :: { SubName A0 }
sub_name
  :  ID     { SubName () $1 }

unit_stmt :: { Decl A0 }
  : srcloc UNIT '::' unit_decl_list  {% srcSpan $1 >>= (\s -> return $ MeasureUnitDef () s $4) }

unit_decl_list :: { [(MeasureUnit, MeasureUnitSpec A0)] }
unit_decl_list
  : unit_decl ',' unit_decl_list  { $1:$3 }
  | unit_decl                     { [$1] }

unit_decl :: { (MeasureUnit, MeasureUnitSpec A0) }
unit_decl
  : srcloc ID '=' unit_spec  {% srcSpan $1 >>= (\s -> return ($2, $4)) }

derived_type_def :: { Decl A0 }
derived_type_def
  : srcloc derived_type_stmt private_sequence_stmt component_def_stmt_list end_type_stmt
  {% do { sp <- srcSpan $1;
	  name <- cmpNames (fst $2) $5 "derived type name";
          return (DerivedTypeDef () sp name (snd $2) $3 $4);  } }

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
: ID           { SubName () $1 } 

private_sequence_stmt :: { [Attr A0] }
private_sequence_stmt
: PRIVATE SEQUENCE     { [Private (), Sequence ()] }
| SEQUENCE PRIVATE     { [Sequence (), Private ()] }
| PRIVATE              { [Private ()] }
| SEQUENCE             { [Sequence ()] }
| {- empty -}          { [] }
  
component_def_stmt_list :: { [Decl A0 ] }
component_def_stmt_list
  : component_def_stmt_list component_def_stmt     { $1++[$2] }
  | component_def_stmt                             { [$1] }

component_def_stmt :: { Decl A0 }
component_def_stmt
  : srcloc type_spec_p component_attr_spec_list '::' entity_decl_list  
  {% (srcSpan $1) >>= (\s -> return $ 
		     if isEmpty (fst $3) 
		     then Decl () s $5 ((BaseType () (fst3 $2) (snd $3) (snd3 $2) (trd3 $2)))
		     else Decl () s $5 ((ArrayT () (fst $3) (fst3 $2) (snd $3) (snd3 $2) (trd3 $2)))) }

component_attr_spec_list :: {([(Expr A0, Expr A0)],[Attr A0])}
component_attr_spec_list
  : component_attr_spec_list ',' component_attr_spec       { (fst $1++fst $3,snd $1++snd $3) }
  | {- empty -}                                            { ([],[]) }

component_attr_spec :: { ([(Expr A0, Expr A0)],[Attr A0]) }
component_attr_spec
:  POINTER              { ([],[Pointer ()]) }
| dim_spec              { ($1,[]) }

access_stmt :: { Decl A0 }
access_stmt
: access_spec '::' access_id_list  { AccessStmt () $1 $3 }
| access_spec access_id_list       { AccessStmt () $1 $2 }
| access_spec                      { AccessStmt () $1 [] }
   
access_id_list :: { [GSpec A0] }
access_id_list
  : access_id_list ',' access_id     { $1++[$3] }
  | access_id                        { [$1] }

access_id :: { GSpec A0 }
access_id 
  : generic_spec                     { $1 }
  
generic_spec :: { GSpec A0 }
generic_spec
: srcloc ID				{% srcSpan $1 >>= (\s -> return $ GName () (Var () s [(VarName () $2,[])])) } 
| OPERATOR '(' defined_operator ')'   { GOper () $3 }
| ASSIGNMENT '(' '=' ')'              { GAssg () }
  
data_stmt :: { Decl A0 }
data_stmt
: DATA data_stmt_set_list		{ Data () $2 }
  
data_stmt_set_list :: { [(Expr A0, Expr A0)] }
data_stmt_set_list
  : data_stmt_set_list ',' data_stmt_set	{ $1++[$3] }
  | data_stmt_set        		   	{ [$1] }
  
data_stmt_set :: { (Expr A0, Expr A0) }
data_stmt_set
  : data_stmt_object_list '/' data_stmt_value_list '/'		{ ($1,$3) }

data_stmt_object_list :: { Expr A0 }
data_stmt_object_list
: data_stmt_object_list ',' data_stmt_object   { ESeq ()  (spanTrans $1 $3) $1 $3 }
  | data_stmt_object			       { $1 }

data_stmt_object :: { Expr A0 }
data_stmt_object
  : variable 			{ $1 }
  

data_stmt_value_list :: { Expr A0 }
data_stmt_value_list
: data_stmt_value_list ',' data_stmt_value	{ ESeq () (spanTrans $1 $3) $1 $3 }
  | data_stmt_value				{ $1 }

data_stmt_value :: { Expr A0 }
data_stmt_value
  : primary		                 	{ $1 }
  
  
external_stmt :: { Decl A0 }
external_stmt
: EXTERNAL '::' name_list  { ExternalStmt () $3 }
| EXTERNAL      name_list  { ExternalStmt () $2 }
  
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
  : '**'        { Power () }
  | '*'         { Mul () }
  | '+'         { Plus () }
  | '//'        { Concat () }
  | rel_op      { $1  }
--  | '.NOT.'     { Not () }
  | '.AND.'     { And () }
  | '.OR.'      { Or () } 


namelist_stmt :: { Decl A0 }
namelist_stmt
: NAMELIST namelist_list   { Namelist () $2 }
  
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
  | SUBROUTINE subname srcloc        newline {% (srcSpan $3) >>= (\s -> return $ ($2,Arg () (NullArg ()) s,Nothing)) }
  | prefix SUBROUTINE subname args_p newline { ($3,$4,Just (fst3 $1)) }
  
function_stmt :: { (SubName A0, Arg A0, Maybe (BaseType A0), Maybe (VarName A0)) }
function_stmt
  : prefix FUNCTION subname args_p RESULT '(' id2 ')' newline { ($3,$4,Just (fst3 $1),Just (VarName () $7)) }
  | prefix FUNCTION subname args_p                    newline { ($3,$4,Just (fst3 $1),Nothing) }
  | FUNCTION subname args_p RESULT '(' id2 ')'        newline { ($2,$3,Nothing,Just (VarName () $6)) }
  | FUNCTION subname args_p                           newline { ($2,$3,Nothing,Nothing) }
  
subname :: { SubName A0 }
subname
: ID	   { SubName () $1 }
  
prefix :: { (BaseType A0, Expr A0, Expr A0) }
prefix
  : type_spec  { $1 }
| RECURSIVE  {% srcSpanNull >>= (\s -> return $ (Recursive (), NullExpr () s, NullExpr () s)) }
| PURE       {% srcSpanNull >>= (\s -> return $ (Pure (), NullExpr () s, NullExpr () s)) }
| ELEMENTAL  {% srcSpanNull >>= (\s -> return $ (Elemental (), NullExpr () s, NullExpr () s)) }

args_p :: { Arg A0 }
args_p
: '(' dummy_arg_list srcloc ')' { $2 (spanExtR ($3, $3) 1) }

dummy_arg_list :: { SrcSpan -> Arg A0 }
dummy_arg_list
: dummy_arg_list2        { Arg () $1 }
| {- empty -}            { Arg () (NullArg ()) }

dummy_arg_list2 :: { ArgName A0 } 
dummy_arg_list2
: dummy_arg_list2 ',' dummy_arg   { ASeq () $1 $3 }
| dummy_arg                       { $1 }

dummy_arg :: { ArgName A0 }
dummy_arg
: ID                     { ArgName () $1 }
| '*'                    { ArgName () "*" }
  
assignment_stmt :: { Fortran A0 }
assignment_stmt
: variable '=' expr                                  { Assg () (spanTrans $1 $3) $1 $3 }
| srcloc ID '(' section_subscript_list ')' '=' expr  {% srcSpan $1 >>= (\s -> return $ Assg () s (Var () s [(VarName () $2, $4)]) $7) }



-- moved up to assignment_stmt
variable :: { Expr A0 }
variable
 : srcloc scalar_variable_name_list     {% (srcSpan $1) >>= (\s -> return $ Var () s $2) }

scalar_variable_name :: { (VarName A0, [Expr A0]) }
scalar_variable_name
: ID '(' section_subscript_list ')' { (VarName () $1, $3) }
| ID '(' ')'                        {% srcSpanNull >>= (\s -> return $ (VarName () $1, [NullExpr () s])) }
| ID                                { (VarName () $1, []) }
| TYPE                           { (VarName () "type", []) } -- a bit of a hack but 'type' allowed as var name
                             -- CAUSES REDUCE REDUCE CONFLICTS!
  
scalar_variable_name_list :: { [(VarName A0, [Expr A0])] }
scalar_variable_name_list
  : scalar_variable_name_list '%' scalar_variable_name    { $1++[$3] }
  | scalar_variable_name                                  { [$1] }


-- bound comes through int_expr
subscript :: { Expr A0 }
subscript
  : int_expr                                      { $1 }
  | bound                                         { $1 }

bound :: { Expr A0 }
bound
: expr ':' expr                       { Bound () (spanTrans $1 $3) $1 $3 }
| expr ':'                            {% srcSpanNull >>= (\s' -> return $ Bound () (spanTrans' $1 s') $1 (NullExpr () s')) }
| srcloc ':' expr                     {% (srcSpan $1) >>= (\s@(_, l) -> return $ Bound () s (NullExpr () (l, l)) $3) }

section_subscript_list :: { [Expr A0] }
section_subscript_list
  : section_subscript_list ',' section_subscript  { $1++[$3] }
  | section_subscript                             { [$1] }
  
section_subscript :: { Expr A0 }
section_subscript
: subscript                             { $1 }
| srcloc ID '=' expr			{% srcSpan $1 >>= (\s -> return $ AssgExpr () s $2 $4) }

expr :: { Expr A0 }
expr
  : level_5_expr                                       { $1 }


level_5_expr :: { Expr A0 }
level_5_expr
  : equiv_operand                                      { $1 }

equiv_operand :: { Expr A0 }
equiv_operand
: equiv_operand '.OR.' or_operand                   { Bin () (spanTrans $1 $3) (Or ()) $1 $3 }
  | or_operand                                      { $1 }

or_operand :: { Expr A0 }
or_operand
: or_operand '.AND.' and_operand                    { Bin () (spanTrans $1 $3) (And ()) $1 $3 }
| and_operand                                       { $1 }


and_operand :: { Expr A0 }
and_operand
: level_4_expr                                       { $1 }

level_4_expr :: { Expr A0 }
level_4_expr 
: level_4_expr rel_op level_3_expr                   { Bin () (spanTrans $1 $3) $2 $1 $3 }
| level_3_expr                                       { $1 }


level_3_expr :: { Expr A0 }
level_3_expr 
: level_3_expr '//' level_2_expr                     { Bin () (spanTrans $1 $3) (Concat ()) $1 $3 }
| level_2_expr                                       { $1 }

level_2_expr :: { Expr A0 }
level_2_expr 
: level_2_expr '+' add_operand                       { Bin () (spanTrans $1 $3) (Plus ()) $1 $3  }
| level_2_expr '-' add_operand                       { Bin () (spanTrans $1 $3) (Minus ()) $1 $3 }
| add_operand                                        { $1 }

add_operand :: { Expr A0 }
add_operand 
: add_operand '*' mult_operand                       { Bin () (spanTrans $1 $3) (Mul ()) $1 $3 }
| add_operand '/' mult_operand                       { Bin () (spanTrans $1 $3) (Div ()) $1 $3 }
| mult_operand                                       { $1 }

mult_operand :: { Expr A0 }
mult_operand 
: level_1_expr '**' mult_operand                     { Bin () (spanTrans $1 $3) (Power ()) $1 $3 }
| level_1_expr                                       { $1 }

level_1_expr :: { Expr A0 }
level_1_expr 
: srcloc '-' primary               {% srcSpan $1 >>= (\s -> return $ Unary () s (UMinus ()) $3) }
| srcloc '.NOT.' primary            {% srcSpan $1 >>= (\s -> return $ Unary () s (Not ()) $3) }
| primary                          { $1 }

primary :: { Expr A0 }
primary 
: constant                         { $1 }
| variable                         { $1 }
| array_constructor                { $1 }
| '(' expr ')'                     { $2 }
| srcloc SQRT '(' expr ')'	   {% srcSpan $1 >>= (\s -> return $ Sqrt () s $4) }

-- Bit of a conflict here- not entirely sure when this is needed
-- | srcloc ':'                       {% srcSpan $1 >>= (\s -> return $ Bound () s (NullExpr () s) (NullExpr () s)) }

fields :: { [String] }
fields
  : fields '.' id2                              { $1++[$3] }
  | id2                                         { [$1] }
  
array_constructor :: { Expr A0 }
array_constructor
: srcloc '(/' expr_list '/)'           {% srcSpan $1 >>= (\s -> return $ ArrayCon () s $3) } 

expr_list :: { [Expr A0] }
expr_list
  : expr_list ',' expr          { $1++[$3] }
  | expr                        { [$1] }
  
constant_p :: { Expr A0 }
constant_p
  : constant_p2                        { $1 }
 
constant_p2 :: { Expr A0 }
constant_p2
: srcloc ID             {% srcSpan $1 >>= (\s -> return $ Var () s [(VarName () $2,[])]) }
  
constant :: { Expr A0 }
constant 
  : literal_constant                             { $1 }

literal_constant :: { Expr A0 }
literal_constant 
: srcloc num                      {% (srcSpan $1) >>= (\s -> return $ Con () s $2) }
| srcloc ZLIT                     {% (srcSpan $1) >>= (\s -> return $ ConL () s 'z' $2) }
| srcloc STR			  {% (srcSpan $1) >>= (\s -> return $ ConS () s $2) }
| logical_literal_constant	  { $1 }

--lit_mark :: { Char }
--lit_mark 
--: 'z' { $1 }
--| 'Z' { $1 }
--| 'b' { $1 }
--| 'B' { $1 }
--| 'o' { $1 }
--| 'O'  { $1 }

logical_literal_constant :: { Expr A0 }
logical_literal_constant 
: srcloc '.TRUE.'                  {% (srcSpan $1) >>= (\s -> return $ Con () s  ".TRUE.") }
| srcloc '.FALSE.'                 {% (srcSpan $1) >>= (\s -> return $ Con () s ".FALSE.") }

rel_op :: { BinOp A0 }
  : '=='                           { RelEQ () }
  | '/='                           { RelNE () }
  | '<'                            { RelLT () }
  | '<='                           { RelLE () }
  | '>'                            { RelGT () }
  | '>='                           { RelGE () }

int_expr :: { Expr A0 }
int_expr
  : expr                             { $1 }

do_variable :: { VarName A0 } 
do_variable
: ID                       { VarName () $1 }

do_construct :: { Fortran A0 }
do_construct
  : block_do_construct              { $1 }

block_do_construct :: { Fortran A0 } 
block_do_construct                  
: srcloc do_stmt do_block end_do  {% srcSpan $1 >>= (\s -> return $ For () s (fst4 $2) (snd4 $2) (trd4 $2) (frh4 $2) $3) }

do_stmt :: { (VarName A0, Expr A0, Expr A0, Expr A0) }
do_stmt
  : nonlabel_do_stmt newline        { $1 }

nonlabel_do_stmt :: { (VarName A0, Expr A0, Expr A0, Expr A0) }
nonlabel_do_stmt
  : DO loop_control                  { $2 }
  | DO                                 {% srcSpanNull >>= (\s -> return $ (VarName () "", NullExpr () s, NullExpr () s, NullExpr () s)) }

loop_control :: { (VarName A0, Expr A0, Expr A0, Expr A0) }
loop_control
  : do_variable '=' int_expr ','  int_expr loop_control2  { ($1,$3,$5,$6) }
--  | int_expr comma_int_expr_opt comma_opt WHILE '(' scalar_logical_expr ')'

loop_control2 :: { Expr A0 }
loop_control2
  : ',' int_expr                     { $2 }
| {- empty -}                      {% srcSpanNull >>= (\s -> return $ Con () s "1") }

do_block :: { Fortran A0 }
do_block
  : block                            { $1 }

end_do :: { }
end_do
  : END DO {} 
  | ENDDO  {} 

block :: { Fortran A0 }
block
  : executable_construct_list        { $1 }
 | {- empty -}                      {% srcSpanNull >>= (\s -> return $ NullStmt () s) }
 
execution_part :: { Fortran A0 }
execution_part 
  : executable_construct_list        { $1 }
 | {- empty -}                      {% srcSpanNull >>= (\s -> return $ NullStmt () s) }

executable_construct_list :: { Fortran A0 }
executable_construct_list
: executable_construct newline executable_construct_list { FSeq () (spanTrans $1 $3) $1 $3 }
| executable_construct newline                           { $1 }


executable_construct :: { Fortran A0 }
executable_construct
: srcloc num executable_construct                {% (srcSpan $1) >>= (\s -> return $ Label () s $2 $3) }
--  | case_construct
  | do_construct                                  { $1 }
  | if_construct                                  { $1 }
  | action_stmt                                   { $1 }
--  | forall_construct
--  | where_construct
 

equivalence_stmt :: { Decl A0 }
equivalence_stmt 
: srcloc EQUIVALENCE '(' vlist ')'              {% srcSpan $1 >>= (\s -> return $ Equivalence () s $4) }

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
| srcloc TEXT				          {% srcSpan $1 >>= (\s -> return $ TextStmt () s $2) }

call_stmt :: { Fortran A0 }
call_stmt
: srcloc CALL call_name '(' actual_arg_spec_list ')' {% srcSpan $1 >>= (\s -> return $ Call () s $3 (ArgList () $5)) }
| srcloc CALL call_name '(' ')'                       {% srcSpan $1 >>= (\s -> return $ Call () s $3 (ArgList () (NullExpr () ($1, $1)))) }
| srcloc CALL call_name                             {% srcSpan $1 >>= (\s -> return $ Call () s $3 (ArgList () (NullExpr () ($1, $1)))) }

call_name :: { Expr A0 }
call_name
: srcloc ID                 {% (srcSpan $1) >>= (\s -> return $ Var () s [(VarName () $2,[])]) }  

actual_arg_spec_list :: { Expr A0 }
actual_arg_spec_list
: actual_arg_spec_list ',' actual_arg_spec      { ESeq () (spanTrans $1 $3) $1 $3 }
| actual_arg_spec                               { $1 }

actual_arg_spec :: { Expr A0 }
actual_arg_spec
  : srcloc ID '=' actual_arg                   {% srcSpan $1 >>= (\s -> return $ AssgExpr () s $2 $4) }
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
: srcloc if_then_stmt block end_if_stmt                  
             {% srcSpan $1 >>= (\s -> return $ If () s $2 $3 [] Nothing) }

| srcloc if_then_stmt block else_if_list end_if_stmt 
             {% srcSpan $1 >>= (\s -> return $ If () s $2 $3 $4 Nothing) }

| srcloc if_then_stmt block else_if_list ELSE newline block end_if_stmt    
             {% srcSpan $1 >>= (\s -> return $ If () s $2 $3 $4 (Just $7)) }

--| if_then_stmt block ELSE block end_if_stmt      {% srcSpan $1 (\s -> If s $1 $2 [] (Just $4)) }

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
  : srcloc ALLOCATE '(' allocation_list ',' STAT '=' variable ')'  
             {% srcSpan $1 >>= (\s -> return $ Allocate () s $4 $8) }

  | srcloc ALLOCATE '(' allocation_list ')' 
            {% srcSpanNull >>= (\e -> srcSpan $1 >>= (\s -> return $ Allocate () s $4 (NullExpr () e))) }


allocation_list :: { Expr A0 }
allocation_list
: allocation_list ',' allocation                  { ESeq () (spanTrans $1 $3) $1 $3 }
| allocation                                      { $1 }
| {- empty -}                                     {% srcSpanNull >>= (return . (NullExpr ())) }

allocate_object_list :: { [Expr A0] }
allocate_object_list
  : allocate_object_list ',' allocate_object      { $1++[$3] }
  | allocate_object                               { [$1] }

allocate_object :: { Expr A0 }
allocate_object
: srcloc scalar_variable_name_list              {% srcSpan $1 >>= (\s -> return $ Var () s $2) }

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
: srcloc allocation_var_list                    {% srcSpan $1 >>= (\s -> return $ Var () s $2) }

allocation_var_list :: { [(VarName A0,[Expr A0])] }
allocation_var_list
  : allocation_var_list '%' allocation_var      { $1++[$3]  }
  | allocation_var                              { [$1] }

allocation_var :: { (VarName A0, [Expr A0]) }
allocation_var
: ID '(' allocate_shape_spec_list ')'         { (VarName () $1, $3) }
| ID                                          { (VarName () $1, []) }

backspace_stmt :: { Fortran A0 }
backspace_stmt
: srcloc BACKSPACE expr                       {% srcSpan $1 >>= (\s -> return $ Backspace () s [NoSpec () $3]) }
| srcloc BACKSPACE '(' position_spec_list ')' {% srcSpan $1 >>= (\s -> return $ Backspace () s $4) }

position_spec_list :: { [Spec A0] }
position_spec_list
  : position_spec_list ',' position_spec          { $1++[$3] }
  | position_spec                                 { [$1] }

position_spec :: { Spec A0 }
position_spec
: expr                                          { NoSpec () $1 }
 | srcloc UNIT '=' expr                         { Unit () $4 }
 | srcloc ID '=' expr                           {% case (map (toLower) $2) of
                                                       "iostat" -> return (IOStat () $4)
                                                       s        ->  parseError ("incorrect name in spec list: " ++ s) }

close_stmt :: { Fortran A0 }
close_stmt
: srcloc CLOSE '(' close_spec_list ')'          {% srcSpan $1 >>= (\s -> return $ Close () s $4) }

close_spec_list :: { [Spec A0] }
close_spec_list
  : close_spec_list ',' close_spec                { $1++[$3] }
  | close_spec                                    { [$1] }

close_spec :: { Spec A0 }
close_spec
: expr                                          { NoSpec () $1 }
| UNIT '=' expr                                 { Unit () $3 }
| ID '=' expr                          

{% case (map (toLower) $1) of
      "iostat" -> return (IOStat () $3)
      "status" -> return (Status () $3)
      s        -> parseError ("incorrect name in spec list: " ++ s) }

--external_file_unit :: { Expr A0 }
--external_file_unit
--  : expr                                          { $1 }

continue_stmt :: { Fortran A0 }
continue_stmt
: srcloc CONTINUE                               {% srcSpan $1 >>= (return . (Continue ())) }

cycle_stmt :: { Fortran A0 }
cycle_stmt
: srcloc CYCLE id2                              {% srcSpan $1 >>= (\s -> return $ Cycle () s $3) }
| srcloc CYCLE                                  {% srcSpan $1 >>= (\s -> return $ Cycle () s "") }

deallocate_stmt :: { Fortran A0 }
deallocate_stmt
: srcloc DEALLOCATE '(' allocate_object_list ',' STAT '=' variable ')' 
        {% srcSpan $1 >>= (\s -> return $ Deallocate () s $4 $8) }

| srcloc DEALLOCATE '(' allocate_object_list ')'
        {% srcSpan $1 >>= (\s -> return $ Deallocate () s $4 (NullExpr () s)) }

endfile_stmt :: { Fortran A0 }
endfile_stmt
: srcloc ENDFILE expr                                  {% srcSpan $1 >>= (\s -> return $ Endfile () s [NoSpec () $3]) }
| srcloc ENDFILE '(' position_spec_list ')'            {% srcSpan $1 >>= (\s -> return $ Endfile () s $4) }

exit_stmt :: { Fortran A0 }
exit_stmt
: srcloc EXIT id2                                    {% srcSpan $1 >>= (\s -> return $ Exit () s $3) }
| srcloc EXIT                                        {% srcSpan $1 >>= (\s -> return $ Exit () s "") }

forall_stmt :: { Fortran A0 }
forall_stmt 
: srcloc FORALL forall_header forall_assignment_stmt 
                 {% srcSpan $1 >>= (\s -> return $ Forall () s $3 $4) }

  | srcloc FORALL forall_header newline forall_assignment_stmt_list forall_stmt_end 
                 {% srcSpan $1 >>= (\s -> return $ Forall () s $3 $5) }

forall_stmt_end :: {}
forall_stmt_end 
  : END FORALL       {}
 | {- empty -}       {}

forall_header :: { ([(String,Expr A0,Expr A0,Expr A0)],Expr A0) }
forall_header
  : '(' forall_triplet_spec_list ',' expr ')'   { ($2,$4) }
| '(' forall_triplet_spec_list ')'              {% srcSpanNull >>= (\s -> return ($2, NullExpr () s)) }

forall_triplet_spec_list :: { [(String,Expr A0,Expr A0,Expr A0)] }
forall_triplet_spec_list
  : forall_triplet_spec_list ',' forall_triplet_spec  { $1++[$3]}
  | forall_triplet_spec                               { [$1] }

forall_triplet_spec :: { (String,Expr A0,Expr A0,Expr A0) }
forall_triplet_spec
  : id2 '=' int_expr ':' int_expr ';' int_expr { ($1,$3,$5,$7) }
| id2 '=' int_expr ':' int_expr              {% srcSpanNull >>= (\s -> return ($1,$3,$5,NullExpr () s)) }

forall_assignment_stmt :: { Fortran A0 }
forall_assignment_stmt
: assignment_stmt                               { $1 }
| pointer_assignment_stmt                       { $1 }


forall_assignment_stmt_list :: { Fortran A0 }
forall_assignment_stmt_list 
: forall_assignment_stmt newline forall_assignment_stmt_list { FSeq () (spanTrans $1 $3) $1 $3 }
| forall_assignment_stmt newline                             { $1 }


goto_stmt :: { Fortran A0 }
goto_stmt
: srcloc GOTO num                                    {% srcSpan $1 >>= (\s -> return $ Goto () s $3) }

if_stmt :: { Fortran A0 }
if_stmt
: srcloc IF '(' logical_expr ')' action_stmt       {% srcSpan $1 >>= (\s -> return $ If () s $4 $6 [] Nothing) }

inquire_stmt :: { Fortran A0 }
inquire_stmt
: srcloc INQUIRE '(' inquire_spec_list ')' 
        {% srcSpan $1 >>= (\s -> return $ Inquire () s $4 []) } 
  | srcloc INQUIRE '(' IOLENGTH '=' variable ')' output_item_list 

        {% srcSpan $1 >>= (\s -> return $ Inquire () s [IOLength () $6] $8) }

inquire_spec_list :: { [Spec A0] }
inquire_spec_list
  : inquire_spec_list ',' inquire_spec           { $1++[$3] }
  | inquire_spec                                 { [$1] }

inquire_spec :: { Spec A0 }
inquire_spec
: expr                             { NoSpec () $1 }
| READ '=' variable                { Read () $3 }
| UNIT '=' variable                { Unit () $3 }
| WRITE '=' variable               { WriteSp () $3 }
| ID '=' expr                      {% case (map (toLower) $1) of
                                            "file"        -> return (File ()	  $3)
                                            "iostat"      -> return (IOStat ()     $3)
                                            "exist"       -> return (Exist ()      $3)
                                            "opened"      -> return (Opened ()     $3)
                                            "number"      -> return (Number ()     $3)
                                            "named"       -> return (Named ()      $3)
                                            "name"        -> return (Name ()       $3)
                                            "access"      -> return (Access ()     $3)
                                            "sequential"  -> return (Sequential () $3)
                                            "direct"      -> return (Direct ()     $3)
                                            "form"        -> return (Form ()       $3)
                                            "formatted"   -> return (Formatted ()  $3)
                                            "unformatted" -> return (Unformatted () $3)
                                            "recl"        -> return (Recl    ()   $3)
                                            "nextrec"     -> return (NextRec ()   $3)
                                            "blank"       -> return (Blank   ()   $3)
                                            "position"    -> return (Position ()  $3)
                                            "action"      -> return (Action   ()  $3)
                                            "readwrite"   -> return (ReadWrite () $3)
                                            "delim"       -> return (Delim    ()  $3)
                                            "pad"         -> return (Pad     ()   $3)
                                            s             -> parseError ("incorrect name in spec list: " ++ s) }
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
: srcloc NULLIFY '(' pointer_object_list ')'           {% srcSpan $1 >>= (\s -> return $ Nullify () s $4) }

pointer_object_list :: { [Expr A0] }
pointer_object_list
  : pointer_object_list ',' pointer_object        { $1++[$3] }
  | pointer_object                                { [$1] }

pointer_object :: { Expr A0 }
pointer_object
  : structure_component                           { $1 }

structure_component :: { Expr A0 }
structure_component
  : variable                                      { $1 }

open_stmt :: { Fortran A0 }
open_stmt
: srcloc OPEN '(' connect_spec_list ')'          {% srcSpan $1 >>= (\s -> return $ Open () s $4) }

connect_spec_list :: { [Spec A0] }
connect_spec_list
  : connect_spec_list ',' connect_spec            { $1++[$3] }
  | connect_spec                                  { [$1] }

connect_spec :: { Spec A0 }
connect_spec
: expr                    { NoSpec () $1 }
| UNIT '=' expr           { Unit () $3 }
| ID '=' expr             {% case (map (toLower) $1) of
                                          "iostat"   -> return (IOStat () $3)
                                          "file"     -> return (File () $3)
                                          "status"   -> return (Status () $3)
                                          "access"   -> return (Access () $3)
                                          "form"     -> return (Form () $3)
                                          "recl"     -> return (Recl () $3)
                                          "blank"    -> return (Blank () $3)
                                          "position" -> return (Position () $3)
                                          "action"   -> return (Action () $3)
                                          "delim"    -> return (Delim () $3)
                                          "pad"      -> return (Pad () $3)
                                          s          -> parseError ("incorrect name in spec list: " ++ s) }

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
: srcloc pointer_object '=>' target                    {% srcSpan $1 >>= (\s -> return $ PointerAssg () s $2 $4) }

target :: { Expr A0 }
target
  : expr                                          { $1 }



print_stmt :: { Fortran A0 }
print_stmt
: srcloc PRINT format ',' output_item_list           {% srcSpan $1 >>= (\s -> return $  Print () s $3 $5) }
| srcloc PRINT format                                {% srcSpan $1 >>= (\s -> return $ Print () s $3 []) }

-- also replaces io_unit
format :: { Expr A0 }
format
: expr                                  { $1 }
--  | literal_constant                  { (Con $1) } -- label
| '*'                                   {% srcSpanNull >>= (\s -> return $ Var () s [(VarName () "*",[])]) }

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
: srcloc READ '(' io_control_spec_list ')' input_item_list {% srcSpan $1 >>= (\s -> return $ ReadS () s $4 $6) }
| srcloc READ '(' io_control_spec_list ')'                 {% srcSpan $1 >>= (\s -> return $ ReadS () s $4 []) }


io_control_spec_list :: { [Spec A0] }
io_control_spec_list
: io_control_spec ',' io_control_spec_list      { $1 : $3 }
| io_control_spec                               { [$1] }

-- (unit, fmt = format), (rec, advance = expr), (nml, iostat, id = var), (err, end, eor = label)


io_control_spec :: { Spec A0 } 
io_control_spec
: --format                              { NoSpec () $1 }
'*'                                    {% srcSpanNull >>= (\s -> return $ NoSpec () (Var () s [(VarName () "*", [])])) }
| END '=' label                          { End () $3 }
| io_control_spec_id                     { $1 }

io_control_spec_id :: { Spec A0 }
: variable                               { NoSpec () $1 }
--| UNIT '=' format                      { Unit () $3 }
--| ID '=' format                          {% case (map (toLower) $1) of
--                                                     "fmt"     -> return (FMT () $3)
--                                                     "rec"     -> return (Rec () $3)
--                                                     "advance" -> return (Advance () $3)
--                                                     "nml"     -> return (NML () $3)
--                                                     "iostat"  -> return (IOStat () $3)
--                                                     "size"    -> return (Size () $3)
--                                                     "eor"     -> return (Eor () $3)
--                                                     s         -> parseError ("incorrect name in spec list: " ++ s) }

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
--  | '*'                                           { (Var [(VarName  () "*",[])]) }
--  | internal_file_unit                            { $1 }

label :: { Expr A0 }
label
: srcloc num                       {% (srcSpan $1) >>= (\s -> return $ Con () s $2) }

num :: { String }
num
: NUM { $1 }
| '1' { "1" }

--internal_file_unit :: { Expr A0 }
--internal_file_unit
--  : default_char_variable                         { $1 }

--default_char_variable :: { Expr A0 }
--default_char_variable
--  : variable       { $1 }

namelist_group_name :: { Expr A0 }
namelist_group_name
  : variable           { $1 }


return_stmt :: { Fortran A0 }
return_stmt
: srcloc RETURN                   {% srcSpan $1 >>= (\s -> return $ Return () s (NullExpr () s)) }
| srcloc RETURN int_expr          {% srcSpan $1 >>= (\s -> return $ Return () s $3) }

scalar_default_int_variable :: { Expr A0 }
scalar_default_int_variable
  : variable                                      { $1 }

scalar_default_char_expr :: { Expr A0 }
scalar_default_char_expr
  : expr                                          { $1 }

rewind_stmt :: { Fortran A0 }
rewind_stmt
: srcloc REWIND expr                        {% srcSpan $1 >>= (\s -> return $ Rewind () s [NoSpec () $3]) }
| srcloc REWIND '(' position_spec_list ')'  {% srcSpan $1 >>= (\s -> return $ Rewind () s $4) }



stop_stmt :: { Fortran A0 }
stop_stmt
: srcloc STOP stop_code                  {% srcSpan $1 >>= (\s -> return $ Stop () s $3) }
| srcloc STOP                            {% srcSpan $1 >>= (\s -> return $ Stop () s (NullExpr () s)) }

stop_code :: { Expr A0 }
stop_code
  : constant                                     { $1 }
  


where_stmt :: { Fortran A0 }
where_stmt
: srcloc WHERE '(' mask_expr ')' where_assignment_stmt {% srcSpan $1 >>= (\s -> return $ Where () s $4 $6) }

where_assignment_stmt :: { Fortran A0 }
where_assignment_stmt
  : assignment_stmt                              { $1 }
mask_expr :: { Expr A0 }
mask_expr
  : logical_expr                                 { $1 }



write_stmt :: { Fortran A0 }
write_stmt
: WRITE '(' io_control_spec_list ')' output_item_list  {% srcSpanNull >>= (\s -> return $ Write () s $3 $5) }
| WRITE '(' io_control_spec_list ')'                   {% srcSpanNull >>= (\s -> return $ Write () s $3 []) }

srcloc :: { SrcLoc }  :    {% getSrcLoc' }

{

getSrcLoc' = do (SrcLoc f l c) <- getSrcLoc
                return (SrcLoc f l (c - 1))

-- Initial annotations from parser

-- Type of annotations

type A0 = () 

srcSpan :: SrcLoc -> P (SrcLoc, SrcLoc)
srcSpan l = do l' <- getSrcLoc'
               return $ (l, l')

-- 0-length span at current position

srcSpanNull :: P (SrcLoc, SrcLoc)
srcSpanNull = do l <- getSrcLoc'
                 return $ (l, l)

spanTrans x y = let (l, _) = getSpan x
		    (_, l') = getSpan y
                in (l, l')

spanTrans' x (_, l') = let (l, _) = getSpan x
                       in (l, l')

spanExtendR t x = let (l, l') = getSpan t
                  in (l, SrcLoc (srcFilename l') (srcLine l') (srcColumn l' + x))

spanExtR (l, l') x = (l, SrcLoc (srcFilename l') (srcLine l') (srcColumn l' + x))

spanExtendL t x = let (l, l') = getSpan t
                  in (SrcLoc (srcFilename l) (srcLine l) (srcColumn l - x), l')

happyError :: P a
happyError = parseError "syntax error"

parseError :: String -> P a
parseError m = do srcloc <- getSrcLoc'
		  fail (srcFilename srcloc ++ ": line " ++ show (srcLine srcloc) ++ " column " ++ show (srcColumn srcloc) ++ ": " ++ m ++ "\n")

tokenFollows s = case alexScan ('\0',[],s) 0 of
                    AlexEOF                 -> "end of file"
                    AlexError  _            -> ""
                    AlexSkip  (_,b,t) len   -> tokenFollows t
	            AlexToken (_,b,t) len _ -> take len s

parse :: String -> Program A0
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

expr2array_spec (Bound _ _ e e') = (e, e') -- possibly a bit dodgy- uses undefined
expr2array_spec e = (NullExpr () (getSpan e) , e)

}
