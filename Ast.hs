module Ast where

import Prelude hiding(exp)
import Data.Int(Int32)
import Jenseits

data Prg = Prg MainClass [ClassDecl]
           deriving (Show,Eq)

data MainClass = MainClass Identifier Identifier Exc Stm
               deriving (Show,Eq)

data ClassDecl = ClassDecl Identifier MaybeId [VarDecl] [MetDecl] 
               deriving (Show,Eq)

data VarDecl = VarDecl Type Identifier
             deriving (Show,Eq)

data MetDecl = MetDecl {
  md_type :: Type,
  md_name :: Identifier,
  md_params :: [MetPara],
  md_thros :: Exc,
  md_body :: MetBody
  } deriving (Show,Eq)

data MetPara = MetPara Type Identifier --[(Type,Identifier)]
                      deriving (Show,Eq)

data MetBody = MetBody {
  mb_locVars :: [VarDecl],
  mb_stms :: [Stm],
  mb_retExp :: Exp
  } deriving (Show,Eq)

data Type = TypeIntAry
          | TypeBool
          | TypeInt
          | TypeId Identifier -- Identifier must be a Class name
          deriving (Show,Eq)

type Identifier = String

data MaybeId = MaybeNo | MaybeId
             deriving (Show,Eq)
data Exc = ExcNo | ExcIO
                 deriving (Show,Eq)
                             
data BinOp = OpAnd | OpLess | OpPlus |  OpMinus |  OpTimes | OpDiv
             deriving (Show, Ord, Eq)

data Stm = StmBlock [Stm]
         | StmIfElse Exp Stm Stm
         | StmWhile Exp Stm
         | StmAsnInt Identifier Exp
         | StmAsnArray Identifier Exp Exp
         | StmPrln Exp
         | StmWrite Exp
         deriving (Show,Eq)

data Exp = ExpOp   Exp BinOp Exp
         | ExpAryElm Exp Exp
         | ExpLen Exp
         | ExpFct Exp Identifier [Exp]
         | ExpRead FilePos
         | ExpIntLit FilePos Int32
         | ExpTrue FilePos
         | ExpFalse FilePos
         | ExpId FilePos Identifier
         | ExpThis FilePos
         | ExpNewArray Exp
         | ExpNewObject FilePos Identifier
         | ExpNot Exp 
     --  | Exp() Exp
         deriving (Show,Eq)

instance AttrFilePos Exp where
  -- getFilePos :: a -> FilePos
  getFilePos exp = case exp of
    ExpOp ex _ _    -> getFilePos ex
    ExpAryElm ex _  -> getFilePos ex
    ExpLen ex       -> getFilePos ex
    ExpFct ex _ _ -> getFilePos ex
    ExpRead fp -> fp
    ExpIntLit fp _ -> fp
    ExpTrue   fp -> fp
    ExpFalse  fp -> fp
    ExpId     fp _ -> fp
    ExpThis   fp -> fp
    ExpNewArray  ex -> getFilePos ex
    ExpNewObject fp _ ->  fp
    ExpNot ex -> getFilePos ex
    
