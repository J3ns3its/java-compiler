-- READ ONLY
{-# LANGUAGE FunctionalDependencies  #-}
module Backend where

import Tree(Prg)
import Names(Temp, Label, MonadNameGen)

class MachineInstr i where
  -- control flow graph:
  jumps :: i -> [Label]
  isFallThrough :: i -> Bool
  isLabel :: i -> Maybe Label
  -- activity analysis:
  use  :: i -> [Temp]
  def  :: i -> [Temp]
  -- interference graph:
  isMoveBetweenTemps :: i -> Maybe (Temp, Temp)
  -- for colouring:
  renameInstr :: i -> (Temp -> Temp) -> i
  -- ??
  isAssignmentToTemp :: i -> Maybe Temp

class (MachineInstr i, Show f) => MachineFunction f i | f -> i where
  machineFunctionName :: f -> String
  machineFunctionBody :: f -> [i]
  machineFunctionRename :: f -> (Temp -> Temp) -> f 
  machineFunctionSpill :: MonadNameGen m => f -> [Temp] -> m f

class (MachineInstr i, MachineFunction f i, Show p) => MachinePrg p f i | p -> f i where
  machinePrgFunctions :: p -> [f]
  -- replacing temps with general purpose registers:
  replaceFunctions :: p -> [f] -> p

class (MachineInstr i, MachineFunction f i, MachinePrg p f i) => CodeGen c p f i | c -> p f i where
  codeGen :: MonadNameGen m => c -> Prg -> m p
  allRegisters :: c -> [Temp]
  generalPurposeRegisters :: c -> [Temp]
