module InterferGraph (
  InterferGraph,
  interferGraph,
  ifgEmpty,
  ifgNodes,
  ifgNodeDegree,
  ifgNodeNeighbours,
  ifgRemoveNode,
  ifgDiscardGenPurpRegs,
  ifg_neighbours,
  ifgShow,
  ifgDump,
  cfgShow,
  mkTempNoToTemp,
  tempNoSetToTemps)
  where

import Data.List
import qualified Data.DList as DL
import Data.Tuple (swap)
import qualified Data.Vector as VB
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Jenseits(cross) -- hashMap_lookupAssertEx)
import Backend
import Names
import DirectedGraph
--import Debug.Trace

mkTempNoToTemp :: HashMap Temp Int -> IntMap Temp
mkTempNoToTemp = IntMap.fromList . map swap. HashMap.toList

-- Example:
--   tempNoSetToTemps tempNoToTemp IntSet.fromList[-1, -10] -> [eax, t2]
tempNoSetToTemps :: IntMap Temp -> IntSet -> [Temp]
tempNoSetToTemps tempNoToTemp = map (tempNoToTemp IntMap.!) . IntSet.toList

-- ****************************************************************************
-- Boilerplate
-- ****************************************************************************
interferGraph
  :: MachineInstr i
  => VB.Vector i        -- instrs
  -> (InterferGraph,    -- ifg
      HashMap Temp Int, -- tempToTempNo
      (DirectedGraph, IntMap IntSet)) -- for test purposes: cfg, liveOuts
interferGraph instS = do
  -- 1. control flow graph
  let cfg = mkControlGraph instS
  -- 2. activity analysis
  let (liveOuts, tempToTempNo, instrNoToDef) = mkActivityAna instS cfg
  -- 3. interference graph
  let ifg = VB.ifoldl' (mkIfgInst liveOuts instrNoToDef tempToTempNo) ifgEmpty instS
  -- remove selfreverence from ifg, i.e. self-loops
  let ifgRet = IntMap.mapWithKey (IntSet.delete) $ ifg_neighbours ifg
--  isMoveBetweenTemps 
  -- return
  (InterferGraph ifgRet, tempToTempNo, (cfg, liveOuts))

-- creates IFG for one Instruction
-- Interference Graph doesnt include ESP and EBP
mkIfgInst :: MachineInstr i
          => IntMap IntSet     -- liveOuts
          -> VB.Vector IntSet  -- Defs
          -> HashMap Temp Int  -- tempToTempNo
          -> InterferGraph
          -> Int               -- Instr Idx          
          -> i                 -- Inst
          -> InterferGraph
mkIfgInst liveOuts instrNoToDef tempToTempNo ifgIn idx inst = 
  case isMoveBetweenTemps inst of
    Just (tmp1, tmp2) -> ifgAddNeighbours ifgIn (tempToTempNo HashMap.! tmp1)
                         (IntSet.delete (tempToTempNo HashMap.! tmp2) neighbours)
    Nothing -> IntSet.foldl faux ifgIn (instrNoToDef VB.! idx)
  where
    neighbours = liveOuts IntMap.! idx
    faux :: InterferGraph -> Int -> InterferGraph
    faux ifg d = ifgAddNeighbours ifg d neighbours

mkActivityAna :: MachineInstr i
               => VB.Vector i
               -> DirectedGraph      -- control flow graph
               -> (IntMap IntSet,    -- Live Outs
                   HashMap Temp Int, -- tempToTempNo
                   VB.Vector IntSet) -- instrNoToDef
mkActivityAna instS cfg =
  -- initial worklist with all instruction numbers in reverse(!) order:
  processWorklist [(VB.length instS - 1),(VB.length instS - 2) .. 0]
                  (IntMap.fromList $ zip [0..(VB.length instS) - 1] (repeat IntSet.empty))
                  (IntMap.fromList $ zip [0..(VB.length instS) - 1] (repeat IntSet.empty))
  where
    processWorklist :: [Int] -> IntMap IntSet -> IntMap IntSet->
                        (IntMap IntSet,    -- Live Outs
                         HashMap Temp Int,
                         VB.Vector IntSet)
    -- worklist completed:
    processWorklist [] _ mpOut =
      (mpOut, tempToTempNo, instrNoToDef')
    -- process first element of worklist (node = instrNo):
    processWorklist (node:workTail) mpIn mpOut =
      let workLst' = if (mpIn IntMap.! node) == nodeIn then
            workTail
          else -- extend workLst if the in set has changed:
            predecessors cfg node ++ workTail
            -- This line costs 3m10s for GameOfLife: workTail ++ predecessors cfg node

      -- continue processing with updated work list and updated liveins&outs
      in  processWorklist workLst' (IntMap.insert node nodeIn mpIn)
                                   (IntMap.insert node nodeOut mpOut)
       
      where
        -- update in and out set for 'node' (see slides.pdf page 307):
        nodeOut :: IntSet
        nodeOut = IntSet.unions . map (mpIn IntMap.!) $ successors cfg node
        nodeIn :: IntSet
        nodeIn  = IntSet.union
                    (instrNoToUse' VB.! node)
                    (nodeOut IntSet.\\  (instrNoToDef' VB.! node))

    -- determine list of all temps and assign the numbers -1, -2 to them.
    -- tempToTempNo is the HashMap for mapping form a 'Temp' to its 'tempNo'.
    instrNoToDef = VB.map def instS
    instrNoToUse = VB.map use instS
    tempToTempNo :: HashMap Temp Int
    tempToTempNo = HashMap.fromList $ zip
        ({- list of all Temps: -} sort . nub . concat $
          VB.toList instrNoToDef ++ VB.toList instrNoToUse)
        [-1,-2..]
    -- make two vectors mapping instruction index to IntSet of used Temp[No]s
    instrNoToDef' :: VB.Vector IntSet
    instrNoToDef' = VB.map (IntSet.fromList . map (tempToTempNo HashMap.!)) instrNoToDef
    instrNoToUse' = VB.map (IntSet.fromList . map (tempToTempNo HashMap.!)) instrNoToUse

mkControlGraph :: MachineInstr i
               => VB.Vector i
               -> DirectedGraph
mkControlGraph instS =
  -- make auxilary map for label Lookup
  VB.ifoldl' auxMap emptyGraph instS
    
  where
    lblToInstrNo = VB.ifoldl' auxLbl HashMap.empty instS
       
    auxMap :: MachineInstr i => DirectedGraph -> Int -> i -> DirectedGraph
    auxMap cfg idx ins  = case jumps ins of
                           [] -> addEdges cfg idx fallNode
                           lbS -> addEdges cfg idx (fallNode ++
                                  (map (lblToInstrNo HashMap.!) lbS))
      where
        fallNode :: [Int]
        fallNode = if isFallThrough ins then [idx + 1] else []

    auxLbl :: MachineInstr i => HashMap String Int -> Int -> i -> HashMap String Int
    auxLbl hMap idx ins = case isLabel ins of
      Just lbl -> HashMap.insert lbl idx hMap
      Nothing -> hMap
             
-- ****************************************************************************
-- InterferenceGraph
-- ****************************************************************************
data InterferGraph = InterferGraph {
   ifg_neighbours :: IntMap IntSet
   } 
   deriving Eq
instance Show InterferGraph where
  show = show . IntMap.toAscList . ifg_neighbours

ifgEmpty :: InterferGraph
ifgEmpty = InterferGraph { ifg_neighbours = IntMap.empty }

-- Note - IntMap.keys:
--   Return all keys of the map in ascending order. Subject to list fusion.
ifgNodes :: InterferGraph -> [Int]
ifgNodes ifg = IntMap.keys $ ifg_neighbours ifg

-- Note: The size function of Data.IntSet has cost O(n), i.e. the same as
--       length function for ordinary lists.
ifgNodeDegree :: InterferGraph -> Int -> Int
ifgNodeDegree ifg node = IntSet.size $ (ifg_neighbours ifg) IntMap.! node

ifgNodeNeighbours :: InterferGraph -> Int -> IntSet
ifgNodeNeighbours ifg node = (ifg_neighbours ifg) IntMap.! node

-- for use during graph creation:
ifgAddNeighbours :: InterferGraph -> Int -> IntSet -> InterferGraph
ifgAddNeighbours ifg node ns =
  (InterferGraph $ 
    -- bulk insert "node to ns":
    IntMap.insertWith IntSet.union node ns $
    -- insert for each "n in ns to node":
    IntSet.foldl'
       (\imap n -> IntMap.insertWith IntSet.union n (IntSet.singleton node) imap)
       (ifg_neighbours ifg) ns)

ifgRemoveNode :: Int -> InterferGraph -> InterferGraph
ifgRemoveNode node ifg =
  let ns = (ifg_neighbours ifg) IntMap.! node
  in  InterferGraph $ IntMap.delete node $ -- remove entry for 'node'
         -- cleanup neighbours - adjust cb: "(delete node)" :: IntSet -> IntSet
         IntSet.foldr' (IntMap.adjust (IntSet.delete node)) (ifg_neighbours ifg) ns

-- Discard the general purpose register nodes from the graph (but without
-- removing the general purpose register enries in the neighbour relation
-- ships).
-- Note: Is noop if a general purpose register is not part of the graph
ifgDiscardGenPurpRegs :: [Int] -> InterferGraph -> InterferGraph
ifgDiscardGenPurpRegs genPurpRegs ifg =
  -- IntMap.delete :: Int -> IntMap Int -> IntMap Int
  InterferGraph $ foldr IntMap.delete (ifg_neighbours ifg) genPurpRegs

-- ****************************************************************************
-- print interfergraph with node numbers translated to 'Temp' values.
ifgDump :: IntMap Temp -> InterferGraph -> String
ifgDump tempNoToTemp ifg = show .
--  map (cross ( (tempNoToTemp IntMap.!),
--               sort . map (tempNoToTemp IntMap.!) . IntSet.toList)) .
  map (cross ( (\n -> IntMap.findWithDefault (mkNamedTemp "t?A") n tempNoToTemp),
               sort . map (\n -> IntMap.findWithDefault (mkNamedTemp "t?B") n tempNoToTemp) . IntSet.toList)) .
  IntMap.toDescList . ifg_neighbours $ ifg
  -- Note: uses toDescList because the tempNos are descending: -1, -2, ...1

-- generate dot file.
ifgShow :: InterferGraph
        -> IntMap Temp    -- tempNoToTemp
        -> DL.DList String
ifgShow ifg tempNoToTemp = 
  DL.singleton "graph G {"
    `DL.snoc` "node[shape=circle]"
    `DL.append` ns
    `DL.append` es
    `DL.snoc` "}"
  where
    ns = foldMap (DL.singleton . fmtNode) $ ifgNodes ifg
    es = foldl' (\dl el -> case fmtEdge el of {[] -> dl; s -> dl `DL.snoc` s})
                DL.empty $ IntMap.toList    $ ifg_neighbours ifg

    fmtNode :: Int -> String
    fmtNode tempNo = nd ++ " [label=\"" ++ nd ++ "\n" ++ show tempNo ++ "\"]"
      where
        nd = show $ tempNoToTemp IntMap.! tempNo
    
    -- show only edges were -src < -dst:
    fmtEdge :: (Int, IntSet) -> String
    fmtEdge (src, dsts) =  case filter (>= src) $ IntSet.toList dsts of
      []  -> []
      [d] -> prefix ++ show (tempNoToTemp IntMap.! d)
      ds  -> prefix ++ " { " ++ 
           (intercalate " " . map (show . (tempNoToTemp IntMap.!)) $ ds) ++ " }"
      where
        prefix = show (tempNoToTemp IntMap.! src) ++ " -- "


-- ****************************************************************************
-- Returns output for .dot file for visualizing the control flow graph.
--
-- Nodes: <instruction>
--        [def: <temps>] [use: <temps>]
--        [out: <temps>]
-- Note: out is only shown for nodes with more than one successors
--
-- Edges: out: <temps>
cfgShow :: (Show i, MachineInstr i) =>
           DirectedGraph  -- control flow graph
        -> VB.Vector i    -- instrs
        -> IntMap IntSet  -- liveOuts (key: instrNo)
        -> IntMap Temp    -- tempNoToTemp
        -> DL.DList String
cfgShow g instrs instrNoToLiveOuts tempNoToTemp = 
  DL.singleton "digraph G {"
    `DL.snoc` "node[shape=box]"
    `DL.append` ns
    `DL.append` es
    `DL.snoc` "}"
  where
    ns = foldMap (DL.singleton . fmtNode) $ IntSet.toAscList $ nodes g
    es = foldMap (DL.singleton . fmtEdge) $ IntMap.toList    $ succs g

    fmtLiveOuts :: Int -> String
    fmtLiveOuts instrNo = ("out: " ++ ) . intercalate " " . map show .
      tempNoSetToTemps tempNoToTemp $ instrNoToLiveOuts IntMap.! instrNo

    -- if only one successor exists:
    --  o set weight to 8 (to force vertical alignment)
    -- if more than one successor exists
    --  o show liveOuts
    fmtEdge :: (Int, [Int]) -> String
    fmtEdge (src, dsts) = show src ++ " -> " ++ case dsts of
      [dst] -> show dst ++ " [weight=8,label=\"" ++ fmtLiveOuts src ++ "\"]"
      _     -> "{ " ++ (intercalate " " . map show $ dsts) ++ " }"

    -- Example return value:
    --   "MOV EAX, t2\ndef: t3"
    -- shows also the liveOuts if the node has not exactly one successor
    fmtNode :: Int -> String
    -- use tail to discard the leading tab:
    fmtNode idx =
      show idx ++ " [label=\"" ++ instrTxt ++ 
      (addNewLine $ fmtAttr "def:" (def instr) ++ 
                    fmtAttr "use:" (use instr) )++
      (if length (successors g idx) /= 1 then "\\n" ++ fmtLiveOuts idx else "")++
      "\"]"

      where
        instr = instrs VB.! idx
        instrTxt = case init . show $ instr of -- init: discard '\n' at end
          ('\t': s) -> s
          s         -> s

    -- adds new-line char if argument is non-null:
    addNewLine :: String -> String
    addNewLine s = if null s then s else '\n':s

    -- usage: fmtAttr "def" (def instr)
    fmtAttr :: String -> [Temp] -> String
    fmtAttr attrName tmps = case tmps of
       [] -> ""
       _  -> attrName ++ ": " ++ (intercalate " " . map show $ tmps) ++ " "
-- eof



