module Bbc.Rom (Rom,load,read,bytes) where

import Control.Monad (when)
import Data.Array(Array,(!),listArray)
import Prelude hiding (init,read)
import qualified Data.ByteString as BS (readFile,unpack)

import Bbc.Byte (Byte(..))

data Rom = Rom { path :: FilePath, bytes :: [Byte], bytesA :: Array Int Byte }

instance Show Rom where
  show Rom{path} = "Rom: " <> path

size :: Int
size = 0x4000 -- 16k

load :: FilePath -> IO Rom
load path = do
  byteString <- BS.readFile path
  let bytes = map Byte $ BS.unpack byteString
  when (length bytes /= size) $ error "Rom.load: bad file size"
  return $ Rom { path, bytes, bytesA = listArray (0,size-1) bytes }

read :: Rom -> Int -> Byte
read rom a = if
  | inRange a -> bytesA rom ! a
  | otherwise -> error $ "Rom.read: " <> show a

inRange :: Int -> Bool
inRange a = a >= 0 && a < size
