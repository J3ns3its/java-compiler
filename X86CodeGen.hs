{-# LANGUAGE MultiParamTypeClasses #-}
module X86CodeGen (X86CodeGen(..)) where

--import qualified Data.DList as DL
import Control.Monad.Writer.Strict -- for MonadWriterT

import Backend
import X86Instr
import X86Function
import X86Prg
--import Control.Monad
import Names
import Tree
--import Debug.Trace

-----------------------------------------
-- Code generation
-----------------------------------------

data X86CodeGen = X86CodeGen

instance CodeGen X86CodeGen X86Prg X86Function X86Instr where
  -- allRegisters :: c -> [Temp]
  allRegisters X86CodeGen = [
    tempEAX, tempEBX, tempECX, tempEDX, tempESI, tempEDI, tempESP, tempEBP ]

  -- generalPurposeRegisters :: c -> [Temp]
  generalPurposeRegisters X86CodeGen = [
    tempEAX, tempEBX, tempECX, tempEDX, tempESI, tempEDI ]

  --  codeGen :: MonadNameGen m => c -> Prg -> m p
  codeGen X86CodeGen prg =  mapM treeX86Fun (methods prg) >>= return . (X86Prg )
    where
      treeX86Fun :: MonadNameGen m => Method -> m X86Function
      treeX86Fun (Method metName _ bdy rTmp ) = do
        tmp1 <- nextTemp
        tmp2 <- nextTemp
        tmp3 <- nextTemp
        let prologue = [Unary PUSH (Reg tempEBP),
                     Binary MOV (Reg tempEBP) (Reg tempESP),
                     Binary SUB (Reg tempESP) (Imm 0), -- space for local vars
                     Binary MOV (Reg tmp1) (Reg tempEBX),
                     Binary MOV (Reg tmp2) (Reg tempESI),
                     Binary MOV (Reg tmp3) (Reg tempEDI)]                     
                      
        let epilogue = [Binary MOV (Reg tempEAX) (Reg rTmp), --todo
                     Binary MOV (Reg tempEDI) (Reg tmp3),
                     Binary MOV (Reg tempESI) (Reg tmp2),
                     Binary MOV (Reg tempEBX) (Reg tmp1),
                     Binary MOV (Reg tempESP) (Reg tempEBP),
                     Unary POP (Reg tempEBP), RET]

        (foldM (\is s -> munchStm s >>= return . (is ++)) [] bdy)  >>=
          return . (++ epilogue) . (prologue ++) >>=
          return . X86Function metName  
 
                    
      munchStm :: MonadNameGen m => Stm -> m [X86Instr]
      munchStm stm =
        case stm of
          MOVE exp1 exp2 -> do
            (op1, ins1) <- munchExp exp1 
            (op2, ins2) <- munchExp exp2
            case (op1, op2) of
              -- Mov does not support Mem for both operands at the same time:
              (Mem _, Mem _) -> do
                tmp <- nextTemp
                return $ ins1 ++ ins2 ++ [Binary MOV (Reg tmp) op2,
                                          Binary MOV op1 (Reg tmp)]
              _ -> return $  ins1 ++ ins2 ++ [Binary MOV op1 op2]

          Tree.LABEL label -> return $ [X86Instr.LABEL label]
          JUMP (NAME label) _ -> return [JMP label]
          CJUMP relOp exp1 exp2 lb1 _ -> do
            -- first op is always Reg, never Mem
            (op1, ins1) <- munchExp exp1 >>= constOrMemOpToTemp 
            (op2, ins2) <- munchExp exp2
            return $ ins1 ++ ins2 ++ [Binary CMP op1 op2, J (sRelop relOp) lb1]
          JUMP _ _  -> error $ "error: Jump doesnt have a label as first para\n"
                               ++ show stm
          _ -> error $ "SEQ is not support in: " ++ show stm
        where
          sRelop relOp = case relOp of
            Tree.EQ -> E
            Tree.NE -> X86Instr.NE
            Tree.LT -> L
            Tree.GT -> G
            Tree.LE -> X86Instr.LE
            Tree.GE -> X86Instr.GE
            _ -> error $ "unkown RelOp " ++ show relOp
            
      munchExp (CONST int) = return (Imm (fromIntegral int),[])
      munchExp (TEMP tmp) = return (Reg tmp, [])
      munchExp (PARAM int) = return (Mem $ EffectiveAddress (Just tempEBP)
                                     Nothing (8 + 4 * fromIntegral int), [])
      munchExp (MEM (TEMP tmp)) = return (Mem $ EffectiveAddress (Just tmp) Nothing 0, [])
      munchExp (MEM exp1) = do
        (Reg tmp, ins) <- munchExp exp1 >>= constOrMemOpToTemp
        return (Mem $ EffectiveAddress (Just tmp) Nothing 0, ins)      
  
      munchExp (Tree.CALL (NAME name) expS) = do
        opInsS <-mapM munchExp expS
        let stackP = fromIntegral $ 4* length opInsS
        return $ (Reg tempEAX, aux opInsS ++
                 [X86Instr.CALL name,
                  Binary ADD (Reg tempESP) (Imm stackP)])
          where
            aux :: [(Operand,[X86Instr])] -> [X86Instr]
            aux [] = []
            aux ((op, ins):tailS) = aux tailS ++ ins ++ [Unary PUSH op]

      --DIV -> IDIV -- EDX = EDX ++ EAX / val + rest EAX
      -- Note: argument of IDIV must not be a const!
      munchExp (BINOP DIV exp1 exp2) = do
        (op1, ins1) <- munchExp exp1
        (op2, ins2) <- munchExp exp2 >>= constOpToTemp
        return (Reg tempEAX, ins1 ++ ins2 ++ [ Binary MOV (Reg tempEAX) op1,
                                               CDQ, -- sign extend EAX into EDX register
                                               Unary IDIV op2])
        
      munchExp (BINOP binOp exp1 exp2) = do 
        (op1, ins1) <- munchExp exp1 -- >>= constOpToTemp
        (op2, ins2) <- munchExp exp2
        temp <- nextTemp
        return (Reg temp, ins1 ++ ins2 ++ [Binary MOV (Reg temp) op1, Binary xOp (Reg temp) op2])
        where
          xOp :: BinaryInstr
          xOp = do
              case binOp of
                PLUS -> ADD
                MINUS -> SUB
                MUL -> IMUL
                Tree.AND -> X86Instr.AND
                err -> error ("binop error at parsing: " ++ show err)
      munchExp err = error ("munchExp\nerror at parsing: " ++ show err)

constOpToTemp :: (MonadNameGen m) => (Operand, [X86Instr]) -> m (Operand, [X86Instr])
constOpToTemp (op, ins) =  case op of
  (Imm _) -> do
    tmp <- nextTemp
    return $ (Reg tmp, ins ++ [Binary MOV (Reg tmp) op])
  _ -> return (op, ins)

constOrMemOpToTemp :: (MonadNameGen m) => (Operand, [X86Instr]) -> m (Operand, [X86Instr])
constOrMemOpToTemp (op, ins) =  case op of
  (Reg _) -> return (op, ins)
  _ -> do
    tmp <- nextTemp
    return $ (Reg tmp, ins ++ [Binary MOV (Reg tmp) op])

{-
        [ Unary  PUSH (Reg tempEBP),
                                       Binary MOV  (Reg tempEBP) (Reg tempESP),
                                       X86Instr.LABEL  "L0",
                                       -- 
                                       Unary  PUSH (Imm 65),    -- push function argument
                                       X86Instr.CALL   "L_write",
                                       Binary ADD (Reg tempESP) (Imm 4),  -- pop 1 var from stack
                                       Binary MOV (Reg $ mkNamedTemp "t1000") (Reg tempEAX),
                                       -- 
                                       Unary  PUSH (Imm 66),
                                       X86Instr.CALL   "L_write",
                                       Binary ADD (Reg tempESP) (Imm 4),
                                       Binary MOV (Reg $ mkNamedTemp "t1001") (Reg tempEAX),
                                       -- 
                                       Unary  PUSH (Imm 10),
                                       X86Instr.CALL   "L_write",
                                       Binary ADD (Reg tempESP) (Imm 4),
                                       Binary MOV (Reg $ mkNamedTemp "t1002") (Reg tempEAX),
                                       --
                                       Binary MOV (Reg $ mkNamedTemp "t1003") (Imm 0),
                                       JMP "L1",
                                       --
                                       X86Instr.LABEL "L1",
                                       Binary MOV (Reg tempEAX) (Reg $ mkNamedTemp "t1003"),
                                       Binary MOV (Reg tempEBP) (Reg tempESP),
                                       Unary  POP (Reg tempEBP),
                                       RET
                                     ]
-}  
