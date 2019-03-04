{
module Parser where

import Control.Exception(throw)    
import Jenseits    
import Lexer
import Ast
import Tokens
}


{- '%name' enforces the parsing of the complete input, i.e. until <EOF> -}
%name parse
%tokentype { Token }

%name parse

%token
  '('        { T_LPar $$ }
  ')'        { T_RPar $$ }
  '['        { T_LSqr $$ }
  ']'        { T_RSqr $$ }
  '{'        { T_LBra $$ }
  '}'        { T_RBra $$ }
  boolean               { T_Boolean $$ }
  class                 { T_Class $$ }
  else                  { T_Else $$ }
  extends               { T_Extends $$ }
  false                 { T_False $$ }
  if                    { T_If $$ }
  int                   { T_Int $$ }
  java_io_IOException   { T_IOException $$ }
  length                { T_Length $$ }
  main                  { T_Main $$ }
  new                   { T_New $$ }
  public           { T_Public $$ }
  return           { T_Return $$ }
  static                { T_Static $$ }
  string                { T_String $$ }
  system_out_println    { T_IOPrintln $$ }
  system_out_write      { T_IOWrite $$ }
  system_in_read        { T_IORead $$ }
  this                  { T_This $$}
  throws                { T_Throws $$ }
  void                  { T_Void $$ }
  while                 { T_While $$ }
  true                  { T_True $$ }
  '='        { T_Eq $$ }
  '&&'       { T_BoolAnd $$ }
  '<'        { T_Less $$ }
  '+'        { T_Plus $$ }
  '-'        { T_Minus $$ }
  '*'        { T_Times $$ }
  '/'        { T_Divide $$ }
  ','        { T_Comma $$ }
  ';'        { T_Semicolon $$ }
  '.'        { T_Dot $$ }
  '!'        { T_Exclamation $$ }
  integer    { T_Num $$ }
  identifier { T_Ident $$ }


{- paritaet kleinste zuerst -}
%right '='
%right ';'
%right ',' 
{- %nonassoc '<' -}
%left '&&'
%left '<' 
%left '+' '-'
%left '*' '/'
%right '!'
%left '.'
%right '(' '{' '['
%left  ')' '}' ']'

%%

Prg :: { Prg }
Prg:
MainClass ClassDeclList   { Prg $1 $2 }

MainClass :: { MainClass }
MainClass:
class identifier '{' public static void main '(' string '[' ']' identifier ')' Exc '{' Stm '}' '}'   { MainClass (snd $2) (snd $12) $14 $16 }

ClassDecl :: { ClassDecl }
ClassDecl:
class identifier MaybeId '{' VarDeclList MetDeclList '}'   { ClassDecl (snd $2) $3 $5 $6 }

MaybeId :: { MaybeId }
MaybeId:
  extends identifier { MaybeId }
|                    { MaybeNo }

ClassDeclList :: { [ClassDecl] }
ClassDeclList:
                  { [] }
| ClassDecl ClassDeclList  { $1:$2 }

VarDecl :: { VarDecl }
VarDecl:
Type identifier ';'  { VarDecl $1 (snd $2) }

VarDeclList :: { [VarDecl] }
VarDeclList:
                              { []  }
| VarDeclList VarDecl       { $2:$1 }

MetDecl :: { MetDecl }
MetDecl:
public Type identifier '(' MetParaList ')' Exc '{' MetBody '}'   { MetDecl $2 (snd $3) $5 $7 $9 }

MetDeclList :: { [MetDecl] }
MetDeclList:
  MetDecl                 { [$1] }
| MetDecl  MetDeclList { $1:$2 }

MetPara :: { MetPara }
MetPara:
Type identifier     { MetPara $1 (snd $2) }

MetParaList :: { [MetPara] }
MetParaList:    
                           { [] }
| MetPara                  { [$1] }
| MetParaList ',' MetPara  { $3:$1 }

MetBody :: { MetBody }
MetBody:
VarDeclList StmList return Exp ';'  { MetBody $1 $2 $4 }

 
Type :: { Type }
Type:
  int '[' ']'   { TypeIntAry }
| boolean       { TypeBool }   
| int           { TypeInt }
| identifier    { TypeId (snd $1) } 

  
Exc :: { Exc }
Exc:
 throws java_io_IOException  { ExcIO}
|                     { ExcNo }
  
  
{- $1 erstes element -}
Stm :: { Stm }
Stm:
  '{' StmList '}'                      { StmBlock $2 }
| if '(' Exp ')' Stm else Stm        { StmIfElse $3 $5 $7 }
| while '(' Exp ')' Stm              { StmWhile $3 $5 }
| identifier '=' Exp ';'             { StmAsnInt (snd $1) $3 }
| identifier '[' Exp ']' '=' Exp ';' { StmAsnArray (snd $1) $3 $6 }
| system_out_println '(' Exp ')' ';'  { StmPrln $3 }
| system_out_write '(' Exp ')' ';'   { StmWrite $3 }

StmList :: { [Stm] }
StmList:
              { [] }
| Stm  StmList  { $1:$2 }


Exp :: { Exp }
Exp:
  Exp '&&' Exp                     { ExpOp $1 OpAnd $3 }
| Exp '<' Exp                      { ExpOp $1 OpLess $3 }
| Exp '+' Exp                      { ExpOp $1 OpPlus $3 }
| Exp '-' Exp                      { ExpOp $1 OpMinus $3 }
| Exp '*' Exp                      { ExpOp $1 OpTimes $3 }
| Exp '/' Exp                      { ExpOp $1 OpDiv $3 }
| Exp '[' Exp ']'                  { ExpAryElm $1 $3 }
| Exp '.' length                 { ExpLen $1 }
| Exp '.' identifier '(' ExpList ')' { ExpFct $1 (snd $3) $5 }
| system_in_read '(' ')'           { ExpRead $1}
| integer                          { ExpIntLit (fst $1) (snd $1)}
| true                             { ExpTrue $1}
| false                            { ExpFalse $1}
| identifier                       { ExpId (fst $1) (snd $1)}
| this                             { ExpThis $1}
| new int '[' Exp ']'              { ExpNewArray $4}
| new identifier '(' ')'           { ExpNewObject (fst $2) (snd $2) }
| '!' Exp                          { ExpNot $2 }
| '(' Exp ')'                      { $2 }

ExpList :: { [Exp] }
ExpList:
                      { [] }
| Exp                 { [$1] }
| ExpList ',' Exp     { $3:$1 }


{
happyError :: [Token] -> a
happyError tks = do
    throw (ApplicationException $ "Parse error " ++ lcn ++ " !\n")
    where
      lcn = case tks of
        [] -> "at end of file"
        (tk : _) -> "when processing token " ++ show tk
}
