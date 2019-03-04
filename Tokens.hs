module Tokens where

import Data.Int(Int32)
import Jenseits

data Token     = T_LPar FilePos
               | T_RPar FilePos
               | T_LSqr FilePos
               | T_RSqr FilePos
               | T_LBra FilePos
               | T_RBra FilePos
               | T_Boolean FilePos
               | T_Class FilePos
               | T_Else FilePos
               | T_Extends FilePos
               | T_False FilePos
               | T_If  FilePos
               | T_Int FilePos
               | T_IOException FilePos
               | T_Length FilePos
               | T_Main FilePos
               | T_New FilePos
               | T_Public FilePos
               | T_Return FilePos
               | T_IOPrintln FilePos
               | T_IOWrite FilePos
               | T_IORead FilePos
               | T_Static  FilePos
               | T_String FilePos
               | T_This FilePos
               | T_Throws FilePos
               | T_Void FilePos
               | T_While FilePos
               | T_True FilePos
               --
               | T_BoolAnd  FilePos
               | T_Less FilePos
               | T_Eq FilePos
               | T_Plus FilePos
               | T_Minus FilePos
               | T_Times FilePos
               | T_Divide FilePos
               | T_Comma FilePos
               | T_Semicolon FilePos
               | T_Dot FilePos
               | T_Exclamation FilePos
               | T_Num (FilePos, Int32)
               | T_Ident (FilePos, String)

               deriving (Eq, Read, Show)


  

