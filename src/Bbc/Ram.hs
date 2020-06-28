
module Bbc.Ram (Ram,create,read,write) where

import Prelude hiding (read)
import Data.Array.IO

import Bbc.Byte as Byte

data Ram = Ram { arr :: IOArray Int Byte }

size :: Int
size = 0x8000 -- 32k

create :: IO Ram
create = do
  arr <- newArray (0,size-1) 0
  return $ Ram {arr}

read :: Ram -> Int -> IO Byte
read Ram{arr} i = readArray arr i

write :: Ram -> Int -> Byte -> IO ()
write Ram{arr} i b  = writeArray arr i b
