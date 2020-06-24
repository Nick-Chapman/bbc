module Bbc.Rom (Spec(..),Rom,load,disassemble) where

import Control.Monad (when)
import Data.Array (Array,listArray,(!))
import Prelude hiding (init,read)
import qualified Data.ByteString as BS (readFile,unpack)

import Bbc.Addr (Addr,minusAddr,addrOfHiLo)
import Bbc.Byte (Byte(..))
import Bbc.Six502.Decode (decode,reEncode)
import Bbc.Six502.Disassembler (displayOpLines)

data Spec = Spec
  { path :: FilePath
  , loadA :: Addr
  , vectorExecA :: [Addr]
  } deriving Show

data Rom = Rom { spec :: Spec, bytes :: [Byte], bytesA :: Array Int Byte }

instance Show Rom where
  show Rom{spec=Spec{path}} = "Rom: " <> path

size :: Int
size = 0x4000 -- 16k

load :: Spec -> IO Rom
load spec@Spec{path} = do
  byteString <- BS.readFile path
  let bytes = map Byte $ BS.unpack byteString
  when (length bytes /= size) $ error "Rom.load: bad file size"
  return $ Rom { spec, bytes, bytesA = listArray (0,size-1) bytes }

read :: Rom -> Int -> Byte
read rom a = if
  | inRange a -> bytesA rom ! a
  | otherwise -> error $ "Rom.read: " <> show a

inRange :: Int -> Bool
inRange a = a >= 0 && a < size

disassemble :: Rom -> IO ()
disassemble rom@Rom{spec=Spec{loadA,vectorExecA},bytes} = do
  let ops = decode bytes
  let bytes' = take (length bytes) $ reEncode ops -- in case 1 or 2 extra 0s
  when (bytes /= bytes') $ fail "re-assemble failed"
  let startAs =
        case vectorExecA of
          [] -> [loadA]
          xs -> map (indirect rom) xs
  mapM_ putStrLn $ displayOpLines loadA startAs ops
  return ()

indirect :: Rom -> Addr -> Addr
indirect rom@Rom{spec=Spec{loadA}} a = do
  let i = a `minusAddr` loadA
  let lo = read rom i
  let hi = read rom (i+1)
  addrOfHiLo hi lo
