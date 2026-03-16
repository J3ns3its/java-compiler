{-# LANGUAGE CPP #-}
-- #define TRACE
module RegAlloc (
  regAlloc
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Vector as VB

import Data.List
import Data.Maybe
import Data.Tuple(swap)
#ifdef TRACE
import Debug.Trace
#endif
import InterferGraph
import Backend
import Names
-- import Jenseits

type StackEntry = (Int, IntSet)

-- p -> p
regAlloc :: (MonadNameGen m, MachineInstr i, MachineFunction f i, MachinePrg p f i) =>
            [Temp] -> [Temp] -> p -> m p
regAlloc allRegs genPurpRegs p =
  mapM regAllocFunc (machinePrgFunctions p) >>= return . replaceFunctions p 
  where
    regAllocFunc :: (MonadNameGen m, MachineInstr i, MachineFunction f i) => f -> m (f)
    regAllocFunc funcIn =
#ifdef TRACE
     if trace ("\n\n***** regAlloc " ++ show (machineFunctionName funcIn)) False then undefined else
#endif       
      aux funcIn

    aux :: (MonadNameGen m, MachineInstr i, MachineFunction f i) => f -> m f
    aux func = do
      -- 1. Make interfernce graph
      let (ifg, tempToTempNo, _) = interferGraph . VB.fromList $ machineFunctionBody func
      let tempNoToTemp = mkTempNoToTemp tempToTempNo
#ifdef TRACE
     if trace ("ifg: \n" ++ show (ifgNodes ifg) ++ "\ntempNoToTemp: " ++
              show (sort . HashMap.toList $ tempToTempNo) ++
              "\ncoloursAvail: " ++ show (IntSet.toList coloursAvail) ++
              "\ninitialColourMap: " ++ show (IntMap.toList $ initialColourMap tempToTempNo)) False then undefined else do
#endif       
      -- 2. do simplify and spill steps
      -- Note: we dicard here the generalPurposeRegisters from ifg (keeping relations):
      let genPurpRegNos = mapMaybe (flip HashMap.lookup tempToTempNo) genPurpRegs
      let stack = simplifyAndSpill (length genPurpRegs) (ifgDiscardGenPurpRegs genPurpRegNos ifg) []

      -- 3. initialize colourMap (with all(!) registers) and select colours
      case select stack (initialColourMap tempToTempNo) coloursAvail [] of
        -- no spillnodes found -> colouring completed:
        (colourMap', []) ->
          return $ machineFunctionRename func (convColourMap tempNoToTemp colourMap')

        -- allocate stack space for spill nodes 'spillS':
        (_, spillS) -> do
          is' <- machineFunctionSpill func (map (tempNoToTemp IntMap.!) spillS)
#ifdef TRACE
          if trace ("select - spillNodes: " ++ show spillS ++
                    "\ninstr after machineFunctionSpill:\n" ++ show (is')) False then undefined else
#endif
          aux is'
          
      where
        -- Note: colourNo = 0 <=> first allReg,
        --       colourNo = 1 <=> second allReg, ...
        colourNoToReg = IntMap.fromList $ zip [0..] allRegs
        regToColourNo = HashMap.fromList . map swap $ IntMap.toList colourNoToReg
        
        coloursAvail = IntSet.fromList . map (regToColourNo HashMap.!) $ genPurpRegs

        -- initialColourMap maps tempNo -> colourNo and contains all used(!) genPurRegs
        initialColourMap :: HashMap Temp Int -> IntMap Int
        initialColourMap tempToTempNo =
          let usedGenPurpRegs = filter (flip HashMap.member tempToTempNo) $ genPurpRegs
          in  IntMap.fromList . map (\r -> (tempToTempNo  HashMap.! r,
                                            regToColourNo HashMap.! r)) $ usedGenPurpRegs
        
        -- convert colourMap (Int->Int) = tempNoToColourNo to function (Temp->Temp)
        convColourMap :: IntMap Temp -> IntMap Int -> (Temp -> Temp)
        convColourMap tempNoToTemp colourMap =
#ifdef TRACE
          if trace ("colourMap\n" ++ show (IntMap.toList colourMap)) False then undefined else
#endif            
          let colourMap'' = HashMap.fromList .
                map (\(k,v) -> (tempNoToTemp IntMap.! k,
                                colourNoToReg IntMap.! v)) . IntMap.toList $ colourMap

          in  safeLookUp colourMap''
          where
            safeLookUp hmap key = case HashMap.lookup key hmap of
                       Just j -> j
                       Nothing -> key
            

select :: [StackEntry] -> IntMap Int -> IntSet -> [Int] ->  (IntMap Int, [Int])
select [] colourMap _ spillS = (colourMap, spillS)
select ((key, neighbours):stackS) colourMap colours spillS =
  case IntSet.toList remainingCol of
    -- found spillNode
    [] -> select stackS colourMap colours (key:spillS)
    -- colour succesfull assigned
    (nextCol:_) ->
#ifdef TRACE
      
      if trace ("select node: " ++ show key ++ " -> colour: " ++ show nextCol) False then undefined else
#endif
        select stackS (IntMap.insert key nextCol colourMap) colours spillS
  where
    -- all colours the neighbours dont have
    remainingCol = (colours IntSet.\\) . IntSet.fromList  .
                   mapMaybe (flip IntMap.lookup colourMap) .
                   IntSet.toList $ neighbours
  
    
simplifyAndSpill :: Int -> InterferGraph -> [StackEntry] -> [StackEntry]
simplifyAndSpill knots ifgIn stackIn = do
  let (stack, ifg) = simplify knots ifgIn
  if 
#ifdef TRACE
     trace ("simplify result - ifg nodes: " ++ show (sort . ifgNodes $ ifg) ++
      "\nstack: \n" ++ (unlines . map show $ stack++stackIn)) False then undefined
  else if
#endif    
    ifg == ifgEmpty then
    (stack ++ stackIn)
  else do
    let fnd@(key, _) =
          maximumBy (\(_, a) (_,b) -> compare (IntSet.size a) (IntSet.size b)) .
                                  IntMap.toList $ ifg_neighbours ifg
    simplifyAndSpill knots (ifgRemoveNode key ifg) ((fnd:stack)++stackIn)


  

-- removes all nodes with degree < k and pushes them on "stack"
simplify :: Int -> InterferGraph -> ([StackEntry], InterferGraph)
simplify coloursCnt ifgIn = IntMap.foldlWithKey faux ([], ifgIn) $ ifg_neighbours ifgIn
  where
    faux :: ([StackEntry], InterferGraph) -> Int -> IntSet -> ([StackEntry], InterferGraph)
    faux (stack, ifg) key neighbours =
#ifdef TRACE
      if trace ("simplify - faux - coloursCnt: " ++ show coloursCnt ++
                ", key: " ++ show key ++ ", neighbours: " ++
                show (IntSet.toList neighbours) ++
                "\nifg nodes: " ++ show (sort . ifgNodes $ ifg) ++
                "\nstack: \n" ++ (unlines . map show $ stack)) False then undefined else
#endif        
      if IntSet.size neighbours < coloursCnt
      -- put node on stack and rm from ifg
      then ((key,neighbours):stack, ifgRemoveNode key ifg)
      -- node is left allone
      else (stack, ifg)

-- eof


