{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module X86Instr where

import Names
import Backend (MachineInstr(..))
import Data.Int
--import Debug.Trace

data UnaryInstr = PUSH
                | POP
                | NEG
                | NOT
                | INC
                | DEC
                | IDIV
                  deriving(Eq, Show)

data BinaryInstr = MOV
                | ADD
                | SUB
                | SHL
                | SHR
                | SAL
                | SAR
                | AND
                | OR
                | XOR
                | TEST
                | CMP
                | LEA
                | IMUL
                 deriving(Eq, Show)

data Cond = E | NE | L | LE | G | GE | Z
  deriving(Eq, Show)
data Scale = S2 | S4 | S8 -- possible scaling values for effective addressing
  deriving(Eq, Show)

data EffectiveAddress = EffectiveAddress { base :: Maybe Temp,
                                           indexScale :: Maybe (Temp, Scale),
                                           displacement :: Int }
                                           deriving (Eq)
                      
instance Show EffectiveAddress where
  show ea = case ea of
   (EffectiveAddress (Just tmp1) Nothing d) -> show tmp1 ++ addDisp d
   (EffectiveAddress b is d) -> error $ "invalid EffectiveAddress " ++
             show b ++ " " ++ show is ++ " " ++ show d
   where
     addDisp d
       | d == 0    = ""
       | d > 0     = " + " ++ show d
       | otherwise = " - " ++ show (-d)
  
    
data Operand = Imm Int32
             | Reg Temp
             | Mem EffectiveAddress
             deriving (Eq)
--(Achtung: Das ist die erste Verwendung von mkNamedTemp!!!):
tempEAX :: Temp
tempEAX = mkNamedTemp "eax" -- CALLER save regiser
tempEBX :: Temp
tempEBX = mkNamedTemp "ebx" -- callee save register
tempECX :: Temp
tempECX = mkNamedTemp "ecx" -- CALLER save regiser
tempEDX :: Temp
tempEDX = mkNamedTemp "edx" -- CALLER save regiser
tempESI :: Temp
tempESI = mkNamedTemp "esi" -- callee save register
tempEDI :: Temp
tempEDI = mkNamedTemp "edi" -- callee save regsiter
tempESP :: Temp
tempESP = mkNamedTemp "esp" -- stack pointer
tempEBP :: Temp
tempEBP = mkNamedTemp "ebp" -- base pointer

data X86Instr = Unary UnaryInstr Operand
              | Binary BinaryInstr Operand Operand
              | LABEL Label
              | CDQ
              | JMP Label
              | J Cond Label
              | CALL Label
              | RET
              | NOP
              deriving (Eq)

instance MachineInstr X86Instr where
  -- control flow graph:
  jumps jmp = case jmp of
    JMP lbl -> [lbl]
    J _ lbl -> [lbl]
    _       -> [] 
  isFallThrough ins = case ins of
    JMP _ -> False
    RET   -> False
    _ -> True
  isLabel ins = case ins of
    LABEL jLbl -> Just jLbl
    _ -> Nothing
  -- activity analysis:
  def i                = case i of
    Unary IDIV _ -> [tempEAX, tempEDX]
    Unary PUSH _ -> []
    Unary _ (Reg tmp) -> filterEBPESP tmp    
    Binary TEST _ _ -> []
    Binary CMP _ _ -> []    
    Binary _ (Reg tmp) _ -> filterEBPESP tmp
    CDQ -> [tempEDX]
    CALL _ -> [tempEAX, tempECX, tempEDX]
    _ -> []

  use i                = case i of
    Unary IDIV op1 -> [tempEAX, tempEDX] ++ isReg op1
    Unary POP _ -> []
    Unary _ (Reg tmp) -> filterEBPESP tmp
    -- if first op of MOV is Reg, it mustn't be listed as used
    Binary MOV (Reg _) op2 -> isReg op2 
    Binary _ op1 op2 -> isReg op1 ++ isReg op2
    CDQ -> [tempEDX]
    RET -> [tempESI, tempEDI, tempEBX, tempEAX]
    _ -> []
    where
      isReg :: Operand -> [Temp]
      isReg (Reg tmp) = filterEBPESP tmp
      isReg (Mem mem) = case  base mem of
        Just tmp -> filterEBPESP tmp
        Nothing -> []
      isReg _ = []
      
  -- interference graph:
--  isMoveBetweenTemps :: i -> Maybe (Temp, Temp)
  isMoveBetweenTemps i = case i of
    Binary MOV (Reg tmp1) (Reg tmp2) -> if
      tempESP == tmp1 ||
      tempEBP == tmp1 ||
      tempESP == tmp2 ||
      tempEBP == tmp2 then Nothing
      else Just (tmp1, tmp2)
    _ -> Nothing
  -- for spilling:
  isAssignmentToTemp _ = error "needed later for register allocation"
  -- ??
  -- renameInstr :: i -> (Temp -> Temp) -> i
  renameInstr i tempToTemp = case i of
    Unary  unop op1 -> Unary unop (convOp op1)
    -- Spezialfall slides.pdf:332: Nach der Registerverteilung kann der Code
    -- redundante Verschiebeoperationen der Form t <- t enthalten.
    -- Diese koennen natuerlich entfernt werden.
    Binary MOV (Reg t1) (Reg t2) ->
      let (tnew1, tnew2) = (tempToTemp t1, tempToTemp t2)
      in  if (tnew1 == tnew2) then
             NOP
          else
            Binary MOV (Reg tnew1) (Reg tnew2)
    Binary binop op1 op2 -> Binary binop (convOp op1) (convOp op2)
    _ -> i
    where
      convOp :: Operand -> Operand
      convOp op = case op of
        Reg tmp -> (Reg $ tempToTemp tmp)
        Mem (EffectiveAddress (Just tmp) Nothing dis) ->
          Mem $ EffectiveAddress (Just $ tempToTemp tmp) Nothing dis 
        Mem _ -> error $ "convOp of: " ++ show op
        Imm _ -> op
             
      



filterEBPESP :: Temp -> [Temp]
filterEBPESP tmp | tmp == tempESP || tmp == tempEBP = []
                 | otherwise = [tmp]

instance Show X86Instr where
  show i = case i of
    LABEL label -> label ++ ":\n"
    CALL tok  -> "\tCALL " ++ tok ++ "\n"
    CDQ       -> "\tCDQ\n"
    JMP label -> "\tJMP " ++ label ++ "\n"
    Unary uInstr op -> "\t" ++ show uInstr ++ " " ++ show op ++ "\n"
    Binary bInstr op1 op2 -> "\t" ++ show bInstr ++ " " ++ show op1 ++ ", " ++ show op2 ++ "\n"
    RET -> "\tRET\n"
    J cond lbl -> "\tJ" ++ show cond ++ " " ++ lbl ++ "\n"
    NOP -> "\tNOP\n"



instance Show Operand where
  show op = case op of
    Imm int -> show int
    Reg temp -> show temp
    Mem ea -> "DWORD PTR [" ++ show ea ++ "]" 

