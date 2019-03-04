module TScope (
  TScope,
  tscope_show,
  tscope_initMain,
  tscope_initClsmet,
  tscope_lookupVar,
  tscope_mkExp_objDeref
  ) where

import Data.Int(Int32)
import Data.List
import qualified Data.DList as DL
import qualified Data.HashMap.Strict as HMap
import Control.Monad -- for foldM

import MJData
import qualified Ast as AST
import Names
import Tree

type TSCMap a = HMap.HashMap AST.Identifier a
-- Calls 'error errTxt ++ ident' when lookup fails
lookupError :: String -> AST.Identifier -> TSCMap a -> a
lookupError errTxt ident mp = case HMap.lookup ident mp of
  Nothing -> error $ errTxt ++ " '" ++ ident ++ "'!"
  Just value -> value

tscope_mkExp_objDeref :: TScope -> Exp -> String -> String -> Exp
tscope_mkExp_objDeref tsc addrExp clsName varName = 
  mkExp_deref addrExp $ tscope_getClsVarOffsetError tsc clsName varName

-- *****************************************************************************
-- Scope
-- *****************************************************************************
data TScope = TScope {
  tsc_thisPtr :: Exp, -- PARAM(0)
  tsc_clsInfoMap :: TSCMap ClsInfo,
  tsc_varInfoMap :: TSCMap VarInfo
  } deriving (Eq, Show)

data VarInfo = VarInfo {
  var_exp    :: Exp -- either TEMP or PARAM, or MEM(TEMP/PARAM + offset)
  } deriving (Eq, Show)


type ClsVar = (Int32, AST.Type, AST.Identifier)  -- Int32: offset in bytes

data ClsInfo = ClsInfo {
  cls_id :: Int32,  -- filePosID: llllcccii
  cls_size :: Int,  -- byte
  cls_vars :: [ClsVar]
  } deriving (Eq, Show)

-- Require: variable exists in class.
tscope_getClsVarOffsetError :: TScope -> String -> String -> Int32
tscope_getClsVarOffsetError tsc clsName varName =
  case find (\(_,_,id) -> id == varName) (cls_vars $ tscope_getClsInfoError tsc clsName) of
    Just (offset, _, _) -> offset
    Nothing -> error $ "tscope_getClsVarOffsetError('" ++ clsName ++ "', '" ++
        varName ++ "')"

-- Require: clsName is known
tscope_getClsInfoError :: TScope -> String -> ClsInfo
tscope_getClsInfoError tsc clsName =
  lookupError clsName "tscope_getClsInfoError" (tsc_clsInfoMap tsc)

-- *****************************************************************************
-- init functions
-- *****************************************************************************
-- Note: Does not need the NameGen Monad!
tscope_initMain :: [AST.ClassDecl] -> TScope
tscope_initMain clsDecls = TScope {
    tsc_thisPtr = (PARAM 0),
    tsc_clsInfoMap = foldl' insClsInfo HMap.empty (zip [1..] clsDecls),
    tsc_varInfoMap = HMap.empty
    }
  where
    insClsInfo :: TSCMap ClsInfo -> (Int, AST.ClassDecl) -> TSCMap ClsInfo
    insClsInfo hmap (id, clsDecl) =
      HMap.insert (AST.cls_name clsDecl) (mkClsInfo clsDecls id clsDecl) hmap

mkClsInfo :: [AST.ClassDecl] -> Int -> AST.ClassDecl -> ClsInfo
mkClsInfo clsDecls clsNo clsDecl = ClsInfo {
  cls_id = filePosId (AST.cls_fp clsDecl) clsNo,
  cls_size = 4 * (1 + length varDecls),
  cls_vars = zipWith (\i (AST.VarParDecl _ ty varName) -> (i, ty, varName))
                     ([4,8..]::[Int32]) varDecls
  }
  where 
    varDecls = getVarDecls clsDecls (Just $ AST.cls_name clsDecl)

    -- collect all varDecls of class including base classes
    getVarDecls :: [AST.ClassDecl] -> Maybe String -> [AST.VarDecl]
    getVarDecls clsDecls clsNameMay = case clsNameMay of
        Nothing -> []
        Just name -> case find (\cd -> AST.cls_name cd == name) clsDecls of
            Nothing -> error $ "getVarDecls - unknown class '" ++ name ++ "'!"
            Just cd -> --base class first, than super class:
                       getVarDecls clsDecls (AST.cls_baseClsMay cd) ++ 
                       AST.cls_varDecls cd 
-- *****************************************************************************
-- init scope for a class method
tscope_initClsmet :: TScope -> AST.ClassDecl -> AST.MetDecl -> NameGen TScope
tscope_initClsmet oldSC clsDecl metDecl = do
    let varMap1 = foldl' (insParVar oldSC) HMap.empty
                         (zip [1..] (AST.metD_params metDecl))
    varMap2 <-    foldM  (insLocVar oldSC) varMap1
                         (AST.metB_vars $ AST.metD_body metDecl)
    let varMap3 = foldl' (insClsVar oldSC (AST.cls_name clsDecl)) varMap3
                         (cls_vars $ tscope_getClsInfoError oldSC $ AST.cls_name clsDecl)
    return $ TScope {
      tsc_thisPtr    = tsc_thisPtr oldSC,
      tsc_clsInfoMap = tsc_clsInfoMap oldSC,
      tsc_varInfoMap = varMap3
    }
  where             
    insParVar :: TScope -> TSCMap VarInfo -> (Int, AST.VarParDecl) -> TSCMap VarInfo
    insParVar tsc hmap (parNo, (AST.VarParDecl _ ty varName)) =
      HMap.insert varName (VarInfo (PARAM parNo)) hmap

    insLocVar :: TScope -> TSCMap VarInfo -> AST.VarParDecl -> NameGen (TSCMap VarInfo)
    insLocVar tsc hmap (AST.VarParDecl _ ty varName) = do
      tmp <- nextTemp
      return $ HMap.insert varName (VarInfo (TEMP tmp)) hmap

    insClsVar :: TScope -> String -> TSCMap VarInfo -> ClsVar -> TSCMap VarInfo
    insClsVar tsc clsName hmap (offset, ty, varName) =
      HMap.insert varName (VarInfo $ tscope_mkExp_objDeref tsc (tsc_thisPtr tsc) clsName varName) hmap


-- *****************************************************************************
-- Ensure: Returns TEMP or PARAM expression or 
--   MEM(TEMP I + offset)
tscope_lookupVar :: TScope -> Maybe FilePos -> AST.Identifier -> Exp
tscope_lookupVar tsc fpMay ident =
  case HMap.lookup ident (tsc_varInfoMap tsc) of
    Just (VarInfo exp) -> exp
    
    Nothing -> error $ "tsc_lookupVar - lookup of variable '" ++ ident ++
      "' failed" ++ case fpMay of
        Nothing -> "!"
        Just fp -> " (file pos: " ++ fmtFilePos fp ++ ")!"

-- *****************************************************************************
-- tscope_show
-- *****************************************************************************
tscope_show :: AST.Prg -> DL.DList String
tscope_show prg =
  foldl' (\dl clsInfo -> dl `DL.append` (showClsInfo clsInfo))
      (DL.singleton $
          "\n***** scope used for translation Ast -> Tree *****\n" ++
          "Exp for thisPtr: " ++ show (tsc_thisPtr tscMain) ++ "\n")
      (HMap.toList $ tsc_clsInfoMap tscMain)

  where
    tscMain = tscope_initMain $ AST.prg_clsDecls prg

showClsInfo :: (AST.Identifier, ClsInfo) -> DL.DList String
showClsInfo (clsName, clsInfo) =
  foldl' (\dl clsVar -> dl `DL.snoc` (showClsVar clsVar))
      (DL.singleton $
          "class " ++ clsName ++ ":\n" ++
          "    id:   " ++ fmtFilePosId (cls_id clsInfo) ++ "\n" ++
          "    size: " ++ show (cls_size clsInfo) ++ " byte\n" ++
          "    variables: \n")
      (cls_vars clsInfo)
  where
    showClsVar :: ClsVar -> String
    showClsVar (offset, varType, varName) = 
      "    " ++ padInt 4 offset ++ ":" ++ padR 10 (fmt varType) ++
      "  " ++ varName ++ "\n"

-- ****************************** EOF ******************************************
