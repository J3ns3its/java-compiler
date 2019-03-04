module Jenseits where

import Control.Exception
import qualified Data.DList as DL
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import System.IO
import System.FilePath

-- file position: line and column number
type FilePos = (Int, Int)

class AttrFilePos a where
  getFilePos :: a -> FilePos

fmtFilePos :: FilePos -> String
fmtFilePos (l, c) = "[" ++ show l ++ "," ++ show c ++ "]"

data ApplicationException = ApplicationException String
    deriving (Eq, Show)

instance Exception ApplicationException where
  -- There is nothing to implement for an Exception!

cross :: (a -> c, b -> d) -> (a, b) -> (c, d)
cross (f, g) (x, y) = (f x, g y)

-- Example:
--   writelnFileDL "instrsCfg1.dot" [] (ifgShow cfg) >>=
--     (\fName -> putStrLn $ "Writing assembler to file '" ++ fName ++ "'...")
writelnFileDL :: String -> String -> DL.DList String -> IO(String)
writelnFileDL fileName fileTypeOpt diffList = do
  let fName = case fileTypeOpt of
                [] -> fileName
                _  -> takeFileName $ replaceExtension fileName fileTypeOpt
  --writeFile fName $ foldMap (++ "\n") diffList
  outh <- openFile fName WriteMode -- superseeds existing file
  mapM_ (hPutStrLn outh) diffList
  hClose outh
  return fName

-- ****************************************************************************
hashMap_lookupAssert :: (Hashable a, Eq a, Show a) => a -> HashMap.HashMap a b -> b
hashMap_lookupAssert key hmap = case HashMap.lookup key hmap of
  Nothing -> error $ "HashMap_lookupAssert failed for key '" ++ show key ++ "'!"
  Just val -> val

hashMap_lookupAssertEx
    :: (Hashable a, Eq a, Show a)
    => String              -- contxt for error message
    -> a                   -- key
    -> HashMap.HashMap a b -- map
    -> b
hashMap_lookupAssertEx contxt key hmap = case HashMap.lookup key hmap of
  Nothing -> error $ "Assert failed in " ++ contxt ++
      " for HashMap_lookup for key '" ++ show key ++ "'!"
  Just val -> val

intMap_lookupAssert :: Int -> IntMap.IntMap a -> a
intMap_lookupAssert key imap = IntMap.findWithDefault 
   (error $ "IntMap_lookupAssert failed for key '" ++ show key ++ "'!") key imap
intMap_lookupAssertEx :: String -> Int -> IntMap.IntMap a -> a
intMap_lookupAssertEx contxt key imap = IntMap.findWithDefault 
   (error $ "Assert failed in " ++ contxt ++ " for IntMap_lookup for key '" ++ show key ++ "'!") key imap
  
