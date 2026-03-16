{
module Lexer where

import Control.Exception
import Jenseits  
import Tokens
}

%wrapper "posn"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters
$letter = $alpha

tokens :-
  $white+;

  "//".*;

  -- Multiline comments - Notes:
  -- o [^set] = . # set, that is without newline (because . is any char
  --   except newline)
  -- o The following does not work for unknown reasons:
  --     "/*" ( [^\*] | "\n" | ("*"+[^\*\/]) )* ("*"+"/") ; 
         "/*" ( [^\*] | [\n] | ("*"+[^\*\/]) )* ("*"+"/") ; 

  "("         { \p _ -> T_LPar (mkFilePos p) }
  ")"         { \p _ -> T_RPar (mkFilePos p) }
  "["         { \p _ -> T_LSqr (mkFilePos p) }
  "]"         { \p _ -> T_RSqr (mkFilePos p) }
  "{"         { \p _ -> T_LBra (mkFilePos p) }
  "}"         { \p _ -> T_RBra (mkFilePos p) }
  "boolean"               { \p _ -> T_Boolean (mkFilePos p) }
  "class"                 { \p _ -> T_Class (mkFilePos p) }
  "else"                  { \p _ -> T_Else (mkFilePos p) }
  "extends"               { \p _ -> T_Extends (mkFilePos p) }
  "false"                 { \p _ -> T_False (mkFilePos p) }
  "if"                    { \p _ -> T_If (mkFilePos p) }
  "int"                   { \p _ -> T_Int (mkFilePos p) }
  "java.io.IOException"   { \p _ -> T_IOException (mkFilePos p) }
  "length"                { \p _ -> T_Length (mkFilePos p) }
  "main"                  { \p _ -> T_Main (mkFilePos p) }
  "new"                   { \p _ -> T_New (mkFilePos p) }
  "public"                { \p _ -> T_Public (mkFilePos p) }
  "return"                { \p _ -> T_Return (mkFilePos p) }
  "static"                { \p _ -> T_Static (mkFilePos p) }
  "String"                { \p _ -> T_String (mkFilePos p) }
  "System.out.println"    { \p _ -> T_IOPrintln (mkFilePos p) }
  "System.out.write"      { \p _ -> T_IOWrite (mkFilePos p) }
  "System.in.read"        { \p _ -> T_IORead (mkFilePos p) }
  "this"                  { \p _ -> T_This (mkFilePos p) }
  "throws"                { \p _ -> T_Throws (mkFilePos p) }
  "void"                  { \p _ -> T_Void (mkFilePos p) }
  "while"                 { \p _ -> T_While (mkFilePos p) }
  "true"                  { \p _ -> T_True (mkFilePos p) }

  "="        { \p _ -> T_Eq (mkFilePos p) }
  "&&"       { \p _ -> T_BoolAnd (mkFilePos p) }
  "<"        { \p _ -> T_Less (mkFilePos p) }
  "+"        { \p _ -> T_Plus (mkFilePos p) }
  "-"        { \p _ -> T_Minus (mkFilePos p) }
  "*"        { \p _ -> T_Times (mkFilePos p) }
  "/"        { \p _ -> T_Divide (mkFilePos p) }
  ","        { \p _ -> T_Comma (mkFilePos p) }
  ";"        { \p _ -> T_Semicolon (mkFilePos p) }
  "."        { \p _ -> T_Dot (mkFilePos p) }
  "!"        { \p _ -> T_Exclamation (mkFilePos p) }
  $digit+  { \p s -> T_Num (mkFilePos p, read s) }
  $letter [$letter $digit \_ \']*
           { \p s  -> T_Ident (mkFilePos p, s) }

           
{
mkToken :: Token -> a -> String -> Token
mkToken tok _ _ = tok


mkFilePos :: AlexPosn -> FilePos
mkFilePos (AlexPn _ l c) = (l, c)

-- Modified version of alexScanTokens. Only modification:
--  o throw ApplicationException instead of using 'error'
alexScanTokensMJ str0 = go (alexStartPos,'\n',[],str0)
  where go inp__@(pos,_,_,str) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) ->
                    throw (ApplicationException $ "lexical error at [" ++ (show line) ++ ", " ++ (show column) ++ "]")
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act pos (take len str) : go inp__'
}
