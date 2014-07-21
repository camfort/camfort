{
module Language.Fortran.Lexer where

import Data.Char
import Debug.Trace

import Language.Fortran
import Language.Haskell.ParseMonad

}

%wrapper "basic"

$letter = [a-zA-Z]
$digit = [0-9]
$bin_digit = [01]
$oct_digit = [0-7]
$hex_digit = [0-9A-Fa-f]
$underscore = \_
$currency_symbol = \$
$at_sign = \@
$sign = [\+\-]
$alphanumeric_charactor = [$letter $digit $underscore $currency_symbol $at_sign]

@name = ($letter | $underscore) ($letter | $digit | $underscore | $currency_symbol | $at_sign)*
@digit_string = $digit+
@signed_digit_string = $sign? @digit_string
@line_space = ($white # \n)*

@kind_param = @digit_string | @name
@int_literal_constant = @digit_string (\_ @kind_param)?

@w = @int_literal_constant
@m = @int_literal_constant
@d = @int_literal_constant
@e = @int_literal_constant
@data_edit_desc = (("I"|"B"|"O"|"Z") @w ( \. @m)?) | "F" @w \. @d | (("E"|"EN"|"ES"|"G") @w \. @d ("E" @e)?) | "L" @w | "A" @w? | "D" @w \. @d ("E" @e)? | "R" @w | "Q"

@binary_constant_prefix = ("B" \' $digit+ \')      | ("B" \" $digit+ \")
@octal_constant_prefix  = ("O" \' $digit+ \')      | ("O" \" $digit+ \")
@hex_constant_prefix    = ("Z" \' $hex_digit+ \')  | ("Z" \" $hex_digit+ \")
@binary_constant_suffix = (\' $bin_digit+ \' "B")  | (\" $bin_digit+ \" "B")
@octal_constant_suffix  = (\' $oct_digit+ \' "O")   | (\" $oct_digit+ \""O")
@hex_constant_suffix    = ( \' $hex_digit+ \' "Z") | ( \" $hex_digit+ \" "Z")

$exponent_letter = [EeDd]
@exponent = @signed_digit_string
@significand = (@digit_string \. @digit_string?) | (\. @digit_string)

@real_literal_constant = (@significand ($exponent_letter @exponent)? (\_ @kind_param)?)
		       | (@digit_string $exponent_letter @exponent (\_ @kind_param)?)

--@signed_real_literal_constant = $sign? @real_literal_constant

tokens :-
  \n\# .* $			{ \s -> Text s }
  \n(C|c).*$			{ \s -> NewLine }
  \n				{ \s -> NewLine }
  ($white # \n)+			;
  "#"				{ \s -> Hash }
  "->"				{ \s -> MArrow }
  "=>"				{ \s -> Arrow }
  "**"				{ \s -> OpPower }
  "//"	 			{ \s -> OpConcat }
  ".EQ."    | ".eq." | "=="	{ \s -> OpEQ }
  ".NE."    | ".ne." | "/="	{ \s -> OpNE }
  ".LE."    | ".le." | "<="	{ \s -> OpLE }
  ".GE."    | ".ge." | ">="	{ \s -> OpGE }
  ".NOT."   | ".not."		{ \s -> OpNOT }
  ".AND."   | ".and."		{ \s -> OpAND }
  ".OR."    | ".or."		{ \s -> OpOR }
  ".TRUE."  | ".true."		{ \s -> TrueConst }
  ".FALSE." | ".false."		{ \s -> FalseConst }
  ".EQV."   | ".eqv."		{ \s -> OpEQV }
  ".NEGV."  | ".negv."  	{ \s -> OpNEQV }
  ".LT."    | ".lt." | "<"	{ \s -> OpLT }
  ".GT."    | ".gt." | ">"	{ \s -> OpGT }
  "*"				{ \s -> OpMul }
  "/"				{ \s -> OpDiv }
  "+"				{ \s -> OpAdd }
  "-"				{ \s -> OpSub }
  ","				{ \s -> Comma }
  "(/"				{ \s -> LArrCon }
  "/)"				{ \s -> RArrCon }
  "("				{ \s -> LParen }
  ")"				{ \s -> RParen }
  "="				{ \s -> OpEquals }
  \"(. # \")*\"			{ \s -> StrConst s }
  \'(. # \')*\'			{ \s -> StrConst s }
  "Z"\'(. # \')*\'		{ \s -> LitConst 'z' s }
  "z"\'(. # \')*\'		{ \s -> LitConst 'z' s }
  \'				{ \s -> SingleQuote }
  \.				{ \s -> Period }
  "::"				{ \s -> ColonColon }
  ":"				{ \s -> Colon }
  ";"                           { \s -> SemiColon }
  "$"				{ \s -> Dollar }
  "NULL()"			{ \s -> Key "null" }
  "&"				; -- ignore & anywhere
  "&"$white*\n        		{ \s -> NewLineAmp } -- ; -- ignore & and spaces followed by '\n' (i.e. line sep)
  "!".*$			;
  "%"				{ \s -> Percent }
  "{"				{ \s -> LBrace }
  "}"				{ \s -> RBrace }
  "else" @line_space "if"       { \s -> Key "elseif" }
  @name			        { \s -> if elem (map toLower s) keywords
                                        then Key (map toLower s)
					else ID s  }
  @data_edit_desc		{ \s -> DataEditDest s }
  @real_literal_constant	{ \s -> Num s }
  @binary_constant_prefix	{ \s -> BinConst s }
  @octal_constant_prefix	{ \s -> OctConst s }
  @hex_constant_prefix		{ \s -> HexConst s }
  @binary_constant_suffix	{ \s -> BinConst s }
  @octal_constant_suffix	{ \s -> OctConst s }
  @hex_constant_suffix		{ \s -> HexConst s }
  @digit_string			{ \s -> Num s }

{
-- Each action has type :: String -> Token
		

-- The token type:
data Token = Key String | LitConst Char String | OpPower | OpMul | OpDiv | OpAdd | OpSub | OpConcat
	   | OpEQ | OpNE | OpLT | OpLE | OpGT | OpGE | OpLG
	   | OpNOT | OpAND | OpOR | OpXOR | OpEQV | OpNEQV
	   | BinConst String | OctConst String | HexConst String
	   | ID String | Num String | Comma | Bang | Percent
	   | LParen | RParen | LArrCon | RArrCon | OpEquals | RealConst String | StopParamStart
	   | SingleQuote | StrConst String | Period | Colon | ColonColon | SemiColon
	   | DataEditDest String | Arrow | MArrow | TrueConst | FalseConst | Dollar
	   | Hash | LBrace | RBrace | NewLine | TokEOF | Text String | NewLineAmp
	   deriving (Eq,Show)

-- all reserved keywords, names are matched against these to see
-- if they are keywords or IDs
keywords :: [String]
keywords = ["allocate", "allocatable","assign",
	"assignment","automatic","backspace","block","call", "case",
	"character","close","common","complex","contains","continue","cycle",
	"data","deallocate","default","dimension","do",
	"double","elemental","else","elseif","elsewhere","end", "enddo", "endif", "endfile","entry",
	"equivalence","exit","external",
	"forall","format","function","goto","iolength",
	"if","implicit","in","include","inout","integer","intent","interface",
	"intrinsic","inquire","kind","len","logical","module",
	"namelist","none","nullify",
	"only","open","operator","optional","out","parameter",
	"pause","pointer","precision","print","private","procedure",
	"program","public","pure","real","read","recursive","result",
	"return","rewind","save","select","sequence","sometype","sqrt","stat",
	"stop","subroutine","target","to","then","type",
	"unit","use","volatile","where","write"]

{- old keywords, many will be removed
keywords :: [String]
keywords = ["access","action","advance","allocate","allocatable","assign",
	"assignment","automatic","backspace","blank","block","call","case",
	"character","close","common","complex","contains","continue","cycle",
	"data","deallocate","default","delim","dimension","direct","do",
	"double","elemental","else","elseif","elsewhere","end", "enddo", "endif", "endfile","entry",
	"eor","err","equivalence","exist","exit","external","file","fmt",
	"forall","form","format","formatted","function","goto","iostat","iolength",
	"if","implicit","in","inout","integer","intent","interface",
	"intrinsic","inquire","kind","len","logical","module","number",
	"named","nml","nextrec","namelist","none","nullify","null()",
	"only","open","opened","operator","optional","out","pad","parameter",
	"pause","pointer","position","precision","print","private","procedure",
	"program","pure","real","read","readwrite","rec","recl","recursive","result",
	"return","rewind","save","select","sequence","sequential","sometype","stat",
	"status","stop","subroutine","target","to","then","type","unformatted",
	"unit","use","volatile","where","write"]
-}

lexer :: (Token -> P a) -> P a
lexer = runL lexer'

lexer' :: Lex a Token
lexer' = do s <- getInput 
       	    startToken
            case alexScan ('\0',[],s) 0 of
              AlexEOF             -> return TokEOF 
              AlexError (c,b,s')  -> fail ("unrecognizable token: " ++ show c)
              AlexSkip  (_,b,s') len -> (discard len) >> lexer'
              AlexToken (_,b,s') len act -> do let tok = act (take len s)
                                               case tok of
					         NewLine    -> lexNewline >> (return tok)
					         NewLineAmp -> (discard len) >> lexNewline >> lexer'
                                                 _          -> (discard len) >> (return tok)
}