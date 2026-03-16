{-
 - Canonization of intermediate code
 -}
module Canon(
  canPrg, canMethod
  ) where

import Names
import Tree
--import Debug.Trace
import qualified Data.DList as DL

type Block = (Label,
              DL.DList Stm,
              Stm) -- must be JUMP or CJUMP
type BlockList = [Block]             

-- | Type for canonized statments
-- This is just a type of lists (in reverse order). We do not use standard
-- lists to avoid possible errors due to inverted ordering.
data CanonStm = Empty | Then CanonStm Stm

append :: CanonStm -> CanonStm -> CanonStm
append xs Empty = xs
append xs (ys `Then` y) = append xs ys `Then` y

asList :: CanonStm -> [Stm]
asList = reverse . asListRev
  where
    asListRev Empty = []
    asListRev (xs `Then` x) = x : asListRev xs

-- | Type for canonized expressions
type CanonExp = (CanonStm, Exp)

isConstant :: Exp -> Bool
isConstant (CONST _) = True
isConstant (NAME _ ) = True
isConstant _ = False

extend :: (MonadNameGen m) => CanonExp -> CanonStm -> m CanonExp
extend ce Empty = return ce
extend (cs, ce) ss =
  -- This can be optimized: it uses temps more often than needed.
  if isConstant ce then
    return (append cs ss, ce)
  else do
    t <- nextTemp
    return (append (cs `Then` MOVE (TEMP t) ce) ss, TEMP t)

join :: (MonadNameGen m) => CanonExp -> (CanonStm, a) -> m (CanonStm, (Exp, a))
join ce1 (s2, e2) = do
  (s1', e1') <- extend ce1 s2
  return (s1', (e1', e2))

joinList :: (MonadNameGen m) => [CanonExp] -> m (CanonStm, [Exp])
joinList [] = return (Empty, [])
joinList (eIn : es) = do
  jes <- joinList es
  (s, (e', je)) <- join eIn jes
  return (s, e' : je)

lift :: (MonadNameGen m) => (a -> b) -> (CanonStm, a) -> m (CanonStm, b)
lift f (s, e) = return (s, f e)

lift2 :: (MonadNameGen m) => (Exp -> a -> b) -> CanonExp -> (CanonStm, a) -> m (CanonStm, b)
lift2 f ce1 ce2 = join ce1 ce2 >>= lift (uncurry f)

-- canonization of expression without CALL in the final expression
canExp' :: (MonadNameGen m) => Exp -> m CanonExp
canExp' e =
  do (ss, e') <- canExp e
     case e' of
       CALL _ _ -> do t <- nextTemp
                      return (ss `Then` MOVE (TEMP t) e', TEMP t)
       _ -> return (ss, e')

-- | Canonization of expressions (may contain one toplevel CALL)
canExp :: (MonadNameGen m) => Exp -> m CanonExp

canExp e@(CONST _) = return (Empty, e)

canExp e@(NAME _) = return (Empty, e)

canExp e@(TEMP _) = return (Empty, e)

canExp e@(PARAM _) = return (Empty, e)

canExp (BINOP o e1 e2) =
  do ce1 <- canExp' e1
     ce2 <- canExp' e2
     lift2 (BINOP o) ce1 ce2

canExp (MEM e) =
  do ce1 <- canExp' e
     lift MEM ce1

canExp (CALL f es) =
  do ce <- canExp' f
     ses <- mapM canExp' es >>= joinList
     lift2 CALL ce ses

canExp (ESEQ s e) =
  do cs <- canStm s
     (cse, e') <- canExp e
     return (append cs cse, e')

-- | Canonization of statements
canStm :: (MonadNameGen m) => Stm -> m CanonStm

canStm (MOVE (MEM e1) e2) =
  do ce1 <- canExp' e1
     ce2 <- canExp e2
     (ss, s) <- lift2 (\e1' e2' -> MOVE (MEM e1') e2')  ce1 ce2
     return $ ss `Then` s

canStm (MOVE (TEMP t) e2) =
  do ce2 <- canExp e2
     (ss, s) <- lift (MOVE (TEMP t)) ce2
     return $ ss `Then` s

canStm (MOVE (PARAM i) e2) =
  do ce2 <- canExp e2
     (ss, s) <- lift (MOVE (PARAM i)) ce2
     return $ ss `Then` s

canStm (MOVE (ESEQ s1 e1) e2) =
  do cs1 <- canStm s1
     cs2 <- canStm (MOVE e1 e2)
     return $ append cs1 cs2

canStm (MOVE x y) = error $ "unsupported arg passed to canStm (MOV...): " ++
      show x ++ ", " ++ show y

canStm (JUMP e ls) =
  do ce <- canExp' e
     (ss, s) <- lift (`JUMP` ls) ce
     return $ ss `Then` s

canStm (CJUMP o e1 e2 l1 l2) =
  do ce1 <- canExp' e1
     ce2 <- canExp' e2
     (ss, s) <- lift2 (\x1 x2 -> CJUMP o x1 x2 l1 l2) ce1 ce2
     return $ ss `Then` s

canStm (SEQ []) =
  return Empty

canStm (SEQ (s1 : ss)) =
  do cs1 <- canStm s1
     css <- canStm (SEQ ss)
     return (append cs1 css)

canStm (LABEL l) = return (Empty `Then` LABEL l)

-- | Canonization of functions
canMethod :: (MonadNameGen m) => Method -> m Method
canMethod m = do
  -- jump on andlabel is added at the last block if no jumps preexists
  -- endlabel is added as last Stm in either case
  endLabel <- nextLabel
  body' <- mapM canStm (body m)
    >>= return . concatMap asList
    >>= aux endLabel
    >>= return . (++ [LABEL endLabel]). tracing (LABEL "") []
  return Method { methodname = methodname m,
                    nparams = nparams m,
                    body = body',
                    returnTemp = returnTemp m }
  where
    -- inital call for blockstm, creates label if needed for Blocks
    aux :: (MonadNameGen m) => Label ->  [Stm] -> m BlockList
    aux eL stmAll@(stm1:stmS) = do
      case stm1 of
        LABEL label -> blockStm eL (label, DL.empty, Nothing) stmS
        _ -> do
          lStart <- nextLabel
          blockStm eL (lStart, DL.empty, Nothing) stmAll
    aux _ _ = undefined          
          
-- | Canonization of programs
canPrg :: (MonadNameGen m) => Prg -> m Prg
canPrg p
    = do mm <- mapM canMethod (methods p)
         return Prg { methods = mm }

-- traverses List of Statements
-- if label is found, a new block begins
-- and a jump is added to the previous block
-- if JUMP or CJUMP is found, block ends
blockStm :: (MonadNameGen m)
         => Label  -- endLabel
         -> (Label, DL.DList Stm, Maybe Stm) -- Block that is currently build
         -> [Stm] 
         -> m BlockList 
blockStm endLabel (lb, stmTs, Nothing) [] = do
  return $ [(lb, stmTs, JUMP (NAME endLabel) [endLabel])]
blockStm _ (lb, stmTs, Just jmp) [] = return [(lb, stmTs, jmp)]
blockStm eL ("", _, Nothing) (stm1:stms) =
  case stm1 of
    LABEL label -> blockStm eL(label, DL.empty, Nothing) stms
    _ -> error "Dead Code"
blockStm eL (lb, stmTs, Nothing) (stm1:stms) =
  case stm1 of
    MOVE _ _ -> blockStm eL (lb , DL.snoc stmTs stm1, Nothing) stms
    JUMP _ _  -> blockStm eL ("", DL.empty, Nothing) stms >>=
                 return . ((lb, stmTs, stm1) :)
    CJUMP _ _ _ _ _ ->  blockStm eL ("", DL.empty, Nothing) stms >>=
                        return . ((lb, stmTs, stm1) :)
    LABEL label -> blockStm eL (label, DL.empty, Nothing) stms >>=
                   return . ((lb, stmTs, JUMP (NAME label) [label]) :)
    _ -> undefined
blockStm _ _ _ = undefined

-- blocks are concatinated into list of statements
-- for each JUMP or CJUMP, the Block with the belonging label is attached next
--   if still available
tracing :: Stm -> [Stm] -> BlockList -> [Stm]
tracing stm stmS []  = stmS ++ [stm]
-- Initial call with LABEL "", as each block ends with JUMP or CJUMP 
tracing (LABEL "") stmS ((lb, stmTs, nextJumpStm):blockList) = do
  tracing nextJumpStm (stmS ++ [LABEL lb] ++ (DL.toList stmTs)) blockList
tracing jmpStm@(JUMP (NAME checkLbl) [_]) stmS blockList = do
  let ((lb, stmTs, nextJmpStm):blockS) = getCheckLbl checkLbl blockList []
  if checkLbl == lb then
    tracing nextJmpStm (stmS ++ [LABEL lb] ++ (DL.toList stmTs)) blockS
    else tracing nextJmpStm (stmS ++ [jmpStm] ++ [LABEL lb] ++ (DL.toList stmTs)) blockS
tracing cJmpStm@(CJUMP relOp e1 e2 lbThen lbElse) stmS blockList = do
  let ((lb, stmTs, nextJmpStm):blockS) = getCheckLbl lbElse blockList []
  -- checks if THEN-block still available
  if lb == lbElse then
    tracing nextJmpStm (stmS ++ [cJmpStm] ++ [LABEL lb] ++ (DL.toList stmTs)) blockS
    else do
    -- if ELSE-block still available, negates Operator and switches Labels
    let ((lb', stmTs', nextJmpStm'):blockS') = getCheckLbl lbThen blockList []    
    if lb' == lbThen then
      tracing nextJmpStm' (stmS ++ [CJUMP negOp e1 e2 lbElse lbThen] ++ [LABEL lb'] ++ (DL.toList stmTs')) blockS'
    -- crates dummy ELSE-Label for Jumping to the real one
    else
      tracing (LABEL "") (stmS ++ [cJmpStm] ++ [JUMP (NAME lbElse) [lbElse]]) blockList
  where
    negOp =
      case relOp of
        Tree.LT -> Tree.GE
        Tree.GT -> Tree.LE
        Tree.LE -> Tree.GT
        Tree.GE -> Tree.LT
        Tree.EQ -> Tree.NE

        _ -> error $ "error at parsing " ++ show relOp ++ " while tracing" 
tracing _ _ _ = undefined  

-- finds Block that starts with checkLbl and returns the List of Blocks
-- found Block is first element of that list
getCheckLbl :: Label -> BlockList -> BlockList -> BlockList
getCheckLbl _ [] travBlockS = reverse travBlockS
getCheckLbl checkLbl (nextBlock@(getLb, _, _):getBlockS) travBlockS
  | checkLbl == getLb = (nextBlock: reverse travBlockS ++ getBlockS)
  | otherwise = getCheckLbl checkLbl getBlockS (nextBlock:travBlockS)

-- eof
