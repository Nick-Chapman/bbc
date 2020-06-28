
module Bbc.MM (MM(..),run) where -- memory map

import Bbc.Addr (minusAddr)

import Bbc.Ram (Ram)
import Bbc.Rom (Rom)
import qualified Bbc.Mem as Mem (Effect(..))
import qualified Bbc.Ram as Ram (read,write)
import qualified Bbc.Rom as Rom (read)

data MM = MM { rom1 :: Rom, rom2 :: Rom, ram :: Ram }

run :: MM -> Mem.Effect a -> IO a
run MM{rom1,rom2,ram} eff = loop eff
  where
  loop :: Mem.Effect a -> IO a
  loop = \case
    Mem.Ret x -> return x
    Mem.Bind e f -> do v <- loop e; loop (f v)
    Mem.Write a b -> if
      | a < 0x8000 -> Ram.write ram (a `minusAddr` 0x0) b
      | a < 0xC000 -> error $ "Mem.Write, address in rom1: " ++ show a
      | otherwise -> error $ "Mem.Write, address in rom2: " ++ show a
    Mem.Read a -> if
      | a < 0x8000 -> Ram.read ram (a `minusAddr` 0x0)
      | a < 0xC000 -> pure $ Rom.read rom1 (a `minusAddr` 0x8000)
      | otherwise -> pure $ Rom.read rom2 (a `minusAddr` 0xC000)
