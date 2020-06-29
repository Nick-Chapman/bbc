
module Bbc.MM (MM(..),run) where -- memory map

import Control.Monad (when)
import qualified Data.Char as Char

import Bbc.Addr (Addr,minusAddr,addrLoByte)
import Bbc.Byte (Byte,byteToUnsigned)
import Bbc.Ram (Ram)
import Bbc.Rom (Rom)
import Bbc.Sheila (Sheila)
import qualified Bbc.Mem as Mem (Effect(..))
import qualified Bbc.Ram as Ram (read,write)
import qualified Bbc.Rom as Rom (read)
import qualified Bbc.Sheila as Sheila (read,write,selectedRom)
import Bbc.Six502.Cycles (Cycles)

data MM = MM { rom1 :: Rom, rom2 :: Rom, ram :: Ram, sheila :: Sheila }

run :: Cycles -> MM -> Mem.Effect a -> IO a
run cyc MM{rom1,rom2,ram,sheila} eff = loop eff
  where
  loop :: Mem.Effect a -> IO a
  loop = \case
    Mem.Ret x -> return x
    Mem.Bind e f -> do v <- loop e; loop (f v)

    Mem.Write a b -> if
      | a < 0x8000 -> do
          when onMode7screenMem $ do updateMode7screen a b
          Ram.write ram (a `minusAddr` 0x0) b
      | a < 0xC000 -> error $ "Mem.Write, address in rom1: " ++ show a
      | a >= 0xFE00 && a < 0xFEFF -> Sheila.write cyc sheila (addrLoByte a) b
      | otherwise -> error $ "Mem.Write, address in rom2: " ++ show a
      where
        onMode7screenMem = a >= 0x7C00 && a <= 0x7FE7

    Mem.Read a -> if
      | a < 0x8000 -> Ram.read ram (a `minusAddr` 0x0)
      | a < 0xC000 -> do
          let locationOfBasic = 0xF -- can it go anywhere?
          selected <- Sheila.selectedRom sheila
          if selected == locationOfBasic
            then pure $ Rom.read rom1 (a `minusAddr` 0x8000)
            else pure 0x00
      | a >= 0xFE00 && a < 0xFEFF -> Sheila.read cyc sheila (addrLoByte a)
      | otherwise -> pure $ Rom.read rom2 (a `minusAddr` 0xC000)


updateMode7screen :: Addr -> Byte -> IO ()
updateMode7screen a b = do
  let offset = a `minusAddr` 0x7C00
  let x = offset `mod` 40
  let y = offset `div` 40
  let c :: Char = Char.chr (byteToUnsigned b)
  print (x,y,c)
