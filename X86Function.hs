{-# LANGUAGE MultiParamTypeClasses #-}
module X86Function where

import Data.List
import Control.Monad
import Backend
import X86Instr
import Names

data X86Function = X86Function { x86FunctionName:: String,
                                 x86FunctionBody:: [X86Instr] }
                   
instance Show X86Function where
  show fun = (x86FunctionName fun) ++ ":\n" ++
    (foldl' (\is i -> show i ++ is) [] . reverse $ x86FunctionBody fun)

instance MachineFunction X86Function X86Instr where
  machineFunctionName = x86FunctionName
  machineFunctionBody = x86FunctionBody

  -- machineFunctionRename :: f -> (Temp -> Temp) -> f 
  machineFunctionRename func sigma =
    X86Function (x86FunctionName func)
                (filter (\i -> i /= NOP) . map (flip renameInstr sigma) $ x86FunctionBody func)
  
  -- machineFunctionSpill :: MonadNameGen m => f -> [Temp] -> m f
  machineFunctionSpill func spillS = foldM spillAFunc func spillS

-- spill a single temp
spillAFunc :: MonadNameGen m => X86Function -> Temp -> m X86Function
spillAFunc (X86Function name (i0:i1:Binary SUB (Reg r) (Imm locVarBytes):instS)) tmpToSpill = do
  -- Note: we process the instructions in reverse order thus we can use the
  --       O(1) cons function for lists:
-- if locVarBytes > 10 then
--   error "spillAFunc - caught in loop"
-- else do
  instS' <- foldM spillInstr [] $ reverse instS
  return $ X86Function name (i0:i1:Binary SUB (Reg r) (Imm $ locVarBytes + 4):instS')
  where
    stackVar = (Mem $ EffectiveAddress (Just tempEBP) Nothing (fromIntegral $ -locVarBytes - 4))
    
    spillInstr :: MonadNameGen m => [X86Instr] -> X86Instr -> m [X86Instr]
    spillInstr is i = case (tmpToSpill `elem` def i,
                            tmpToSpill `elem` use i) of
      (True,  False) -> do -- def
        temp <- nextTemp
        return (renInstr temp : Binary MOV stackVar (Reg temp) : is)
      (False, True) -> do -- use
        temp <- nextTemp
        return (Binary MOV (Reg temp) stackVar : renInstr temp : is)
      (True,  True)  -> do -- def & use
        temp <- nextTemp
        return (Binary MOV (Reg temp) stackVar : renInstr temp : Binary MOV stackVar (Reg temp): is)
      (False, False) -> return (i:is)
      where
        renInstr tmpNew = renameInstr i (\t -> if t == tmpToSpill then tmpNew else t)
spillAFunc _ _ = undefined
