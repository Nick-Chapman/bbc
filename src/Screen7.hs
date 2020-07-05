
-- Peek into the BBC Ram to where the mode-7 screen normally lives
module Screen7 (Screen7,fromRam,lines) where

import Prelude hiding (lines)
import Data.Char as Char (chr)

import Bbc.Byte (byteToUnsigned)
import Bbc.Ram (Ram)
import qualified Bbc.Ram as Ram (read)

data Screen7 = Screen7 { lines :: [String] }

fromRam :: Ram -> IO Screen7
fromRam ram = do
  let
    getLine i = do
      bytes <-  sequence [Ram.read ram a | a <- take 40 [0x7C00 + 40*i .. ]]
      return [ Char.chr $ byteToUnsigned b | b <- bytes ]
  Screen7 <$> sequence [getLine i | i <- [0..24] ]
