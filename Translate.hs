module Translate where

import Control.Exception(throw)
import Data.Int(Int32)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

--import Data.List
--import Control.Monad
--import qualified Data.HashMap.Strict as HMap
--import Debug.Trace

import Jenseits
import qualified Ast as A
import Tree
import Names
import Scope
import Prelude hiding(exp, LT, EQ)

-- exception codes (as passed to L_raise())
data ExceptCode = Except_New_SizeInvalid      -- 0
                | Except_Array_NotInitialized -- 1 array var was not initialized
                | Except_ArrayIdx_Negative    -- 2
                | Except_ArrayIdx_TooBig      -- 3
                | Except_Obj_NotInitialized   -- 4 object was not initialized
                deriving (Eq, Show, Enum)
-- make exception nr which 'contains' also the file pos
-- format: llllcccii
mkExceptNo :: FilePos -> ExceptCode -> Int32
mkExceptNo (l, c) code = fromIntegral $ (l * 1000 + c) * 100 + fromEnum code

mkRaiseStm :: Temp -> A.Exp -> ExceptCode -> Stm
mkRaiseStm temp exp xcode = 
  MOVE (TEMP temp) (CALL (NAME "L_raise") [CONST $ mkExceptNo (getFilePos exp) xcode])

-- ****************************************************************************
translate :: A.Prg -> NameGen Prg 
translate (A.Prg mainClass classDeclS) = do
  let scMain = scope_InitMain classDeclS
  classD <- (mapM (translateClass scMain) classDeclS)
  mainC <- (translateMain scMain mainClass)
  return (Prg $ mainC : concat classD) 
  
translateMain :: Scope -> A.MainClass -> NameGen Method
translateMain sc (A.MainClass _ _ _ xs) = do
  temp <- nextTemp
  stm <- translateStm sc xs
  chkNonInit sc $ chkNonInitStm sc HashSet.empty xs
  return $ Method "Lmain" 1 [stm, MOVE (TEMP temp) (CONST 0)] (temp)

translateClass :: Scope -> A.ClassDecl -> NameGen [Method]
translateClass scMain (A.ClassDecl clsName baseClassOpt varDeclS metDeclS) = do
  meths <- mapM  (translateMet scMain clsName varDeclS) metDeclS
  chkCls
  return meths
  where
    chkCls :: NameGen()
    chkCls = case baseClassOpt of
      A.MaybeNo -> return ()
      A.MaybeId -> 
        throw (ApplicationException $
          "Inheritance is not supported but class " ++ clsName ++
          " has a base class!")

translateMet :: Scope -> String -> [A.VarDecl] -> A.MetDecl -> NameGen Method
translateMet scMain clsName clsVars metDecl = do
  sc <- scope_InitMet scMain clsName clsVars metDecl
  bodyStms <- mapM (translateStm sc) (A.mb_stms $ A.md_body metDecl)
  temp <- nextTemp
  (_,retExp) <- translateExp sc (A.mb_retExp $ A.md_body metDecl)
  chkNonInit sc $ chkNonInitStm sc (chkNonInitExp sc (A.mb_retExp $ A.md_body metDecl))
                            (A.StmBlock  (A.mb_stms $ A.md_body metDecl))
  return $ Method (clsName ++ "$" ++ A.md_name metDecl)
                  (1 + (length $ A.md_params metDecl))
                  (bodyStms ++ [MOVE (TEMP temp) retExp])
                  temp

-- ****************************************************************************************************
translateStm :: Scope -> A.Stm -> NameGen Stm
translateStm sc (A.StmBlock xs) = do
  seqs <- mapM (translateStm sc) xs  -- MapM muss im Monadischen Kontext Aufgerufen werden
  return $ SEQ seqs -- Parameter von Return ist nicht im Monadischen Kontext

translateStm sc (A.StmIfElse exp stmT stmF) = do
  (_, expT) <- translateExp sc exp
  stmTtrue <- translateStm sc stmT
  stmTfalse <- translateStm sc stmF
  lTrue <- nextLabel
  lFalse <- nextLabel
  lExit <- nextLabel
  return $ SEQ [
             CJUMP EQ expT (CONST 1) lTrue lFalse,
             LABEL lTrue,
             stmTtrue,
             JUMP (NAME lExit) [lExit],
             LABEL lFalse,
             stmTfalse,
             LABEL lExit]
             
  

translateStm sc (A.StmWhile exp stm) = do
  (_, expWhile) <- translateExp sc exp
  stmWhile <- translateStm sc stm
  lInit <- nextLabel
  lStart <- nextLabel
  lExit <- nextLabel  
  return $ SEQ [
               LABEL lInit,
               CJUMP EQ expWhile (CONST 1) lStart lExit,
               LABEL lStart,
               stmWhile,
               JUMP (NAME lInit) [lInit],
               LABEL lExit
               ]

translateStm sc (A.StmAsnInt varName exp) = do
  (_,expT) <- translateExp sc exp
  let (VarInfo varExp _ _) = scope_LookUpVarAndChk sc varName (getFilePos exp)
  return $ MOVE varExp expT

translateStm sc (A.StmAsnArray varName idxExp valExp) = do
  (_, idExpTree) <- translateExp sc idxExp
  (_, valExpTree) <- translateExp sc valExp
  lTrue1 <- nextLabel -- is not used for local vars
  lTrue2 <- nextLabel
  lTrue3 <- nextLabel    
  lError1 <- nextLabel -- is not used for local vars
  lError2 <- nextLabel  
  lError3 <- nextLabel
  tempIdExp <- nextTemp
  tempLocal <- nextTemp
  let (VarInfo aryExp _ varsc) = scope_LookUpVarAndChk sc varName (getFilePos idxExp)
  let chkUninitVarStms = if varsc /= VarClass then
        -- check not needed, this is hanlded by the chkNonInit.. functions
        -- looking for uninitialized local variables. And function parameters
        -- are always considered as been initialized
        []
      else
        [CJUMP Tree.NE aryExp (CONST 0) lTrue1 lError1,  
         LABEL lError1,
         mkRaiseStm tempLocal idxExp Except_Array_NotInitialized,
         LABEL lTrue1]

  return $ SEQ ((MOVE (TEMP tempIdExp) idExpTree) : chkUninitVarStms ++
        [CJUMP Tree.GE (TEMP tempIdExp) (CONST 0) lTrue2 lError2,
          LABEL lTrue2,
          CJUMP LT (TEMP tempIdExp) (MEM aryExp) lTrue3 lError3,
          LABEL lError2,
          mkRaiseStm tempLocal idxExp Except_ArrayIdx_Negative,    
          LABEL lError3,
          mkRaiseStm tempLocal idxExp Except_ArrayIdx_TooBig,
          LABEL lTrue3,
          MOVE (MEM $ BINOP PLUS aryExp (BINOP MUL (CONST 4) (BINOP PLUS (CONST 1) (TEMP tempIdExp))))
          valExpTree
        ])
    
translateStm sc (A.StmPrln exp1) = do
  temp <-nextTemp
  (_,exp) <- (translateExp sc exp1)
  return $ MOVE (TEMP temp) (CALL (NAME "L_println_int") [exp])
                                           
translateStm sc (A.StmWrite exp1) = do
  temp <-nextTemp
  (_,exp) <- (translateExp sc exp1)
  return $ MOVE (TEMP temp) (CALL (NAME "L_write") [exp])
                                           
--translateStm _ stm = error $ "translateExp - not yet implemented: " ++ show stm
       
                                    
-- ****************************************************************************************************
translateExp :: Scope -> A.Exp -> NameGen (A.Type, Exp)
translateExp sc (A.ExpOp exp1 A.OpLess exp2) = do
  (expT1, expT2) <- assert sc "OpLess" A.TypeInt exp1 exp2
  lTrue <- nextLabel
  lFalse <- nextLabel
  temp <- nextTemp
  return $  (A.TypeBool, ESEQ ( SEQ [MOVE (TEMP temp) (CONST 0),
             CJUMP LT  expT1 expT2 lTrue lFalse,
             LABEL lTrue,
             MOVE (TEMP temp) (CONST 1),
             LABEL lFalse])
             $ TEMP temp)
  
translateExp sc (A.ExpOp exp1 A.OpAnd exp2) = do
  (rexp1, rexp2) <- assert sc "OpAnd" A.TypeBool exp1 exp2
  lTrue1 <- nextLabel
  lTrue2 <- nextLabel  
  lExit <- nextLabel  
  temp <- nextTemp
  return 
    (A.TypeBool, ESEQ (SEQ [
             MOVE (TEMP temp) (CONST 0), -- initialise returnvalue
             CJUMP EQ rexp1 (CONST 1) lTrue1 lExit, -- if first parameter is false, secondis irrelevant
             LABEL lTrue1,
             CJUMP EQ rexp2 (CONST 1) lTrue2 lExit,
             LABEL lTrue2,
             MOVE (TEMP temp) (CONST 1),
             LABEL lExit])
             (TEMP temp)
    ) -- Tupel
    
translateExp sc (A.ExpOp exp1 binop exp2) = do
  let (retTy, op) = (translateOp binop)
  (rexp1, rexp2) <- assert sc "binary operation" A.TypeInt exp1 exp2
  return (retTy, BINOP op rexp1 rexp2)
  {-
  if typ1 /= A.TypeInt then
    error $ "type check failed at (line, col) for BinOp: found " ++ show typ1 ++ " when expecting " ++ show A.TypeInt ++ "!"
  else if typ2 /= A.TypeInt then
    error $ "type check failed at (line, col) for BinOp: found " ++ show typ2 ++ " when expecting " ++ show A.TypeInt ++ "!"
  else
    return (retTy, BINOP op rexp1 rexp2)
  -}
  where
    translateOp :: A.BinOp -> (A.Type, BinOp)
--    translateOp A.OpAnd   = (A.TypeBool, AND)
    translateOp A.OpPlus  = (A.TypeInt, PLUS)
    translateOp A.OpMinus = (A.TypeInt, MINUS)
    translateOp A.OpTimes = (A.TypeInt, MUL)
    translateOp A.OpDiv   = (A.TypeInt, DIV)
    translateOp _ = do error "unknow BINOP"

translateExp sc (A.ExpAryElm aryExp idxExp) = do
  (_ , aryExpTree) <- translateExp sc aryExp
  (_ , idxExpTree) <- translateExp sc idxExp  
  return (A.TypeInt, MEM (BINOP PLUS aryExpTree (BINOP MUL (CONST 4) (BINOP PLUS (CONST 1) idxExpTree))))
translateExp sc (A.ExpLen aryExp) = do
  (expTy, aryExpTree) <- translateExp sc aryExp
  assert1 "array length expression" aryExp A.TypeIntAry expTy
  -- todo Except_Array_NotInitialized,
  temp <- nextTemp
  return (A.TypeInt, ESEQ (MOVE (TEMP temp) (MEM aryExpTree)) (TEMP temp))

translateExp sc (A.ExpFct exp1 metName params) = do
  (typ, exp) <- translateExp sc exp1
  expS <- mapM (translateExp sc) params
  lTrue <- nextLabel
  lError <- nextLabel
  tempExp <- nextTemp
  tempRaise <- nextTemp
  case typ of
    A.TypeId clsName -> do
      let clsMetName = clsName ++ "$" ++ metName
      return (scope_LookUpMetType sc clsMetName,
             ESEQ (SEQ [
             MOVE (TEMP tempExp) exp,
             CJUMP Tree.NE (TEMP tempExp) (CONST 0) lTrue lError,
             LABEL lError,
             mkRaiseStm tempRaise exp1 Except_New_SizeInvalid,    
             LABEL lTrue])
              ((CALL (NAME clsMetName)) $ (TEMP tempExp : (snd <$> expS))))
    _ -> throw (ApplicationException $
                errorOut exp1 "class method call" (A.TypeId "<className>") typ)

translateExp _ (A.ExpRead _) = 
  return (A.TypeInt, CALL (NAME "L_read") [])


translateExp  _ (A.ExpIntLit _ int1) =
  return (A.TypeInt, CONST int1)

translateExp  _ (A.ExpTrue _) =
  return (A.TypeBool, CONST 1)

translateExp  _ (A.ExpFalse _) =
  return (A.TypeBool, CONST 0)

translateExp sc (A.ExpId fp varName ) = do
  let varInfo = scope_LookUpVarAndChk sc varName fp
  return (var_type varInfo, var_exp varInfo)

translateExp sc (A.ExpThis _) = do
  let clsName = sc_clsName sc 
  return $ (A.TypeId clsName, PARAM 0)

-- Actions:
--  o call halloc
--  o return address of allocated memory  
translateExp sc (A.ExpNewArray sizeExp) = do 
  (_, lengthAry) <- translateExp sc sizeExp
  lTrue <- nextLabel
  lError <- nextLabel
  tempLenAry <- nextTemp
  tempAryPtr <- nextTemp
--  _ <- trace ("arraylength:" ++ show lengthAry ++ "\n\n" ++ scope_show sc) translateExp sc sizeExp
  return
    ( A.TypeIntAry
    , ESEQ ( SEQ [ -- allocate array:
             MOVE (TEMP tempLenAry) lengthAry,
             CJUMP Tree.GE (TEMP tempLenAry) (CONST 0) lTrue lError,
             LABEL lError,
             mkRaiseStm tempAryPtr sizeExp Except_Array_NotInitialized,    
             LABEL lTrue,
             MOVE (TEMP tempAryPtr) (CALL (NAME "L_halloc")
               [BINOP MUL (CONST 4) (BINOP PLUS (TEMP tempLenAry) (CONST 1))])
               -- store length in first field:
               , MOVE (MEM (TEMP tempAryPtr)) (TEMP tempLenAry)
             ]
           )
           (TEMP tempAryPtr)
    )

-- Actions:
--  o lookup 'clsName' to get <class size>
--  o call halloc
--  o return address of allocated memory
translateExp sc (A.ExpNewObject _ clsName) = do
  temp1 <- nextTemp
  let clsInfo = scope_LookUpClass sc clsName 
  return $ (A.TypeId clsName,
            ESEQ (SEQ [MOVE (TEMP temp1) (CALL (NAME "L_halloc") [CONST $ cls_size clsInfo]),
                       MOVE (MEM (TEMP temp1)) (CONST $ cls_id clsInfo)])
                 (TEMP temp1)
           )

translateExp sc (A.ExpNot exp) = do
  (_, exp') <- translateExp sc exp
--  (tthis, tthat) <- assert A.TypeBool expType 
  return (A.TypeBool, BINOP MINUS (CONST 1) exp')
  
--translateExp _ exp = error $ "translateExp - not yet implemented: " ++ show exp


-- *********************************** TypeChecking *******************************************
assert1 :: String -> A.Exp -> A.Type -> A.Type -> NameGen()
assert1 conText posExp expectedType foundType =
  if expectedType /= foundType then
     throw (ApplicationException $ errorOut posExp conText expectedType foundType)
  else
     return ()


assert :: Scope -> String -> A.Type -> A.Exp -> A.Exp -> NameGen(Exp,Exp)
assert sc conText expectedType exp1 exp2 = do
  (typeExp1, rexp1) <- (translateExp sc exp1)
  (typeExp2, rexp2) <- (translateExp sc exp2)
  if expectedType /= typeExp1 then
    throw (ApplicationException $ errorOut exp1
           (conText ++ " in Exp 1 '" ++ show exp1 ++ "'") expectedType typeExp1)
  else if expectedType /= typeExp2 then 
    throw (ApplicationException $ errorOut exp2
           (conText ++ " in Exp 2 '" ++ show exp2 ++ "'") expectedType typeExp2)
  else return (rexp1, rexp2)

errorOut :: A.Exp -> String -> A.Type -> A.Type -> String
errorOut exp conText expectedType actualType =
  "Type checking error at " ++ fmtFilePos (getFilePos exp) ++
  ":\n  expected Type '" ++ (show expectedType) ++
  "'\n  did not match actual Type '" ++ (show actualType) ++ "'\n  for " ++ conText


-- ****************************************************************************
-- Check for noninitialized local variables.
-- ****************************************************************************
--  o We use a set containing all used but not initialized variables and
--    process the statements from last to first.
--  o Variables are added whenever the are used.
--  o Variables are removed when they get a value assigned:
--      - StmVarAsn
--      - StmAryElemAsn

-- checks if refVars contains any local var names which might not have
-- been initialized. If found returns error text.
chkNonInit
  :: Scope          -- refVars
  -> HashSet String -- list of used but not assigned local(!) variables
  -> NameGen()
chkNonInit scope refVars =
  if refVars == HashSet.empty then
    return ()
  else
    throw $ ApplicationException (concat . map fmtErr . HashSet.toList $ refVars)
  where
    fmtErr :: String -> String
    fmtErr ident =
      --let varInfo = scope_LookUpVar scope ident
          "Error - variable '" ++ ident ++ "'" ++
          -- " declared at " ++ fmtFilePos (-1, -1) ++
          " in function '" ++ scope_getMetName scope ++ 
          "' might not be initialized before first use!\n"
  
chkNonInitExp ::
    Scope -> A.Exp
    -> HashSet String -- list of used but not assigned local(!) variables
chkNonInitExp scope expIn = case expIn of
  A.ExpFct exp1 _ expList ->  HashSet.unions . map (chkNonInitExp scope) $ (exp1:expList)
  A.ExpId fp id1 -> case var_scope $ scope_LookUpVarAndChk scope id1 fp of
    VarLocal -> HashSet.singleton id1
    _ -> HashSet.empty
  A.ExpOp exp1 _ exp2 -> aux2 exp1 exp2
  A.ExpAryElm exp1 exp2 -> aux2 exp1 exp2
  A.ExpLen exp1      -> chkNonInitExp scope exp1
  A.ExpNewArray exp1 -> chkNonInitExp scope exp1
  A.ExpNot exp1      -> chkNonInitExp scope exp1
  _ -> HashSet.empty
  where
    aux2 :: A.Exp -> A.Exp -> HashSet String
    aux2  exp1 exp2 =
      HashSet.union (chkNonInitExp scope exp1) (chkNonInitExp scope exp2)

  --  used: ExpIdent, assigned: StmAsn, StmAryAsn
chkNonInitStm :: Scope -> HashSet String -> A.Stm -> HashSet String
chkNonInitStm sc refVars stmIn = case stmIn of
  A.StmBlock stmList -> foldl (chkNonInitStm sc) refVars $ reverse stmList
  A.StmIfElse exp1 stm1 stm2 ->
    HashSet.unions $ (chkNonInitExp sc exp1):
                     (chkNonInitStm sc refVars stm1) :
                     [chkNonInitStm sc refVars stm2]
  A.StmWhile exp1 stm1 ->
    HashSet.unions $ (chkNonInitExp sc exp1):
                     [chkNonInitStm sc refVars stm1]
  A.StmAsnInt id1 exp1 ->
    HashSet.delete id1 $ HashSet.union refVars $ chkNonInitExp sc exp1
  A.StmAsnArray id1 exp1 exp2 -> HashSet.delete id1 $ HashSet.union refVars
                $ HashSet.union (chkNonInitExp sc exp1) (chkNonInitExp sc exp2)
  A.StmPrln exp1  -> HashSet.union refVars $ chkNonInitExp sc exp1
  A.StmWrite exp1 -> HashSet.union refVars $ chkNonInitExp sc exp1
  
--    Note: Statement are traversed in reverse order
--fold chkNonInitStm scope HashSet.empty stnList
--  chkNonInit Scope -> HashSet String -> ()
--     throw ApplicationException... filePos
  
-- *********************************** EOF ****************************************************
