module Scope (
  Scope(sc_clsName),
  scope_show,
  scope_InitMain,
  scope_InitMet,
  VarInfo(..),
  VarScope(..),
  ClsInfo(..),
  scope_LookUpVar,
  scope_LookUpVarAndChk,
  scope_LookUpClass,
  scope_LookUpMetType
  ) where

import qualified Data.HashMap.Strict as HMap
import Data.List
import Data.Int(Int32)
import Control.Monad
import Control.Exception(throw)
--import Debug.Trace

import Jenseits
import Tree
import Names
import qualified Ast as A

type SCMap a = HMap.HashMap String a

data ClsInfo = ClsInfo {
  cls_id :: Int32,
  cls_size :: Int32
  } deriving Show

data VarInfo = VarInfo {
  var_exp :: Exp,
  var_type :: A.Type,     -- needed for NEW?
  var_scope :: VarScope
  } deriving Show
data VarScope = VarLocal | VarClass | VarParam
  deriving (Eq)
instance Show VarScope where
  show vs = case vs of
    VarLocal -> "local variable"
    VarClass -> "class member variable"
    VarParam -> "function paramter"
    
data Scope = Scope {
  sc_name    :: String,
  sc_clsName :: String, -- empty for main class
  sc_clsInfo :: SCMap ClsInfo,
  sc_metType :: SCMap A.Type,
  sc_varInfo :: SCMap VarInfo  -- all infos needed by 'ExpId <identidifer>'  
  } deriving Show

scope_show :: Scope -> String
scope_show sc =
  "***** scope for " ++ sc_name sc ++ " *****\n" ++
  "class info:\n" ++
  (concat . map (\c -> "  " ++ show c ++ "\n"). HMap.toList) (sc_clsInfo sc) ++
  "function info:\n" ++
  (concat . map (\f -> "  " ++ show f ++ "\n"). HMap.toList) (sc_metType sc) ++
  "variable info:\n" ++
  (concat . map (\v -> "  " ++ show v ++ "\n"). HMap.toList) (sc_varInfo sc) 

lookupUnsafe :: Show a => String -> String -> SCMap a -> a
lookupUnsafe callerCtx ident hm = case HMap.lookup ident hm of
  Nothing -> error $ "lookupUnsafe failed for " ++ callerCtx ++ ": " ++ ident
     ++ "\n" ++ (concat . map show . HMap.toList) hm
  Just v  -> v
  
-- ******************************************SCOPE INIT************************************************
scope_InitMain :: [A.ClassDecl] -> Scope
scope_InitMain classDeclS =
  Scope {
    sc_name = "Main",
    sc_clsName = "",
    sc_clsInfo = foldl' clsFoldFun HMap.empty $ (zip [1..] classDeclS),
    sc_metType = foldl' metFoldFun HMap.empty $ classDeclS,
    sc_varInfo = HMap.empty  --foldl' varFoldFun HMap.empty $ classDeclS
    }
  where
    {-
     varFoldFun :: SCMap VarInfo -> A.ClassDecl -> SCMap VarInfo
     varFoldFun hMap (A.ClassDecl _ _ varDeclS _) =
       foldl' varFoldAux hMap varDeclS
     varFoldAux :: SCMap VarInfo -> A.VarDecl -> SCMap VarInfo
     varFoldAux hMap (A.VarDecl varType varId) =
       HMap.insert varId (VarInfo (Const varType) hMap
     -}
     clsFoldFun :: SCMap ClsInfo -> (Int32, A.ClassDecl) -> SCMap ClsInfo
     clsFoldFun hMap (clsId, A.ClassDecl clsName _ varDeclS _) =
       HMap.insert clsName (ClsInfo clsId (fromIntegral$ 4* (1 + length varDeclS))) hMap
       
     metFoldFun :: SCMap A.Type -> A.ClassDecl -> SCMap A.Type
     metFoldFun hMap (A.ClassDecl clsName _ _ metDeclS) =
       foldl' metFoldAux hMap (zip metDeclS $ repeat clsName)
     metFoldAux :: SCMap A.Type -> (A.MetDecl,A.Identifier) -> SCMap A.Type
     metFoldAux hMap ( metDecl,clsName) =
       HMap.insert (clsName ++ "$" ++ A.md_name metDecl) (A.md_type metDecl) hMap
        
scope_InitMet :: Scope -> String -> [A.VarDecl] -> A.MetDecl -> NameGen Scope
scope_InitMet scMain clsName clsVarS metDecl = do
  -- 1. add class variables to HashMap (-> sc_varInfo):
  let hm  = foldl' clsVarFoldFun HMap.empty $ zip [4,8..] clsVarS
  -- 2. add local variables to HashMap (-> sc_varInfo):
  hm' <- foldM locVarFoldFun hm $ (A.mb_locVars . A.md_body) metDecl  
  return $ Scope {
    sc_name = clsName ++ " - " ++ A.md_name metDecl,
    sc_clsName = clsName,
    sc_clsInfo = sc_clsInfo scMain,
    sc_metType = sc_metType scMain,
    -- 3. add function parameters to HashMap (-> sc_varInfo):
    sc_varInfo = foldl' paraFoldFun hm' $ zip [1..] $ A.md_params metDecl
    

    }
  where
     clsVarFoldFun :: SCMap VarInfo -> (Int32, A.VarDecl) -> SCMap VarInfo
     clsVarFoldFun hMap (offSet, A.VarDecl varType varId) =
       HMap.insert varId (VarInfo (MEM (BINOP PLUS (PARAM 0) (CONST offSet))) varType VarClass) hMap
       
     paraFoldFun :: SCMap VarInfo -> (Int, A.MetPara) -> SCMap VarInfo
     paraFoldFun hMap (paraNr, A.MetPara paraType paraId) =
       HMap.insert paraId (VarInfo (PARAM (fromIntegral paraNr)) paraType VarParam) hMap
       
     locVarFoldFun :: SCMap VarInfo -> A.VarDecl -> NameGen (SCMap VarInfo)
     locVarFoldFun hMap (A.VarDecl varType varId) = do
       temp <- nextTemp
       return $ HMap.insert varId (VarInfo (TEMP temp) varType VarLocal) hMap
       

     
     
-- ******************************************SCOPE Lookup**********************************************

scope_LookUpVar :: Scope -> String -> VarInfo  
scope_LookUpVar sc varName = 
  lookupUnsafe "scope_LookupVar" varName $ sc_varInfo sc

-- check for unknown variables
scope_LookUpVarAndChk :: Scope -> String -> FilePos -> VarInfo  
scope_LookUpVarAndChk sc varName fp =
  case HMap.lookup varName $ sc_varInfo sc of
    Just varInfo -> varInfo
    Nothing -> throw (ApplicationException $
         "Type checking error at " ++ fmtFilePos fp ++
         ":\n  found undeclared variable '" ++ varName ++ "' !")
  
scope_LookUpClass :: Scope -> String -> ClsInfo
scope_LookUpClass sc clsName =
  lookupUnsafe "scope_LookupClass" clsName $ sc_clsInfo sc

scope_LookUpMetType :: Scope -> String -> A.Type
scope_LookUpMetType sc metName =
  lookupUnsafe "scope_LookupType" metName $ sc_metType sc


--classInsertHm :: [(String, ClsInfo)] -> HMap.HashMap String ClsInfo
--classInsertHm datas =
--  foldl' (\hm (str, info) -> HMap.insert str info hm) HMap.empty datas 

{-
classShow :: HMap.HashMap String ClsInfo -> IO()
classShow hm = mapM_ prfFun $ HMap.toList hm
  where
    prfFun :: (String, ClsInfo) -> IO ()
    prfFun (str, info) = putStrLn $ str ++ " " ++ show info
-}
