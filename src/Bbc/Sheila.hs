
module Bbc.Sheila (Sheila,create,read,write,selectedRom) where

import Prelude hiding (read)
import Data.IORef

import Bbc.Byte as Byte
import Bbc.Six502.Cycles (Cycles)

data Sheila = Sheila { selectedRomRef :: IORef Byte }

selectedRom :: Sheila -> IO Byte
selectedRom Sheila{selectedRomRef} = do
  b <- readIORef selectedRomRef
  --putStrLn $ "selectedRom -> " ++ show b
  return b

create :: IO Sheila
create = do
  selectedRomRef <- newIORef 0x0
  return Sheila { selectedRomRef }

read :: Cycles -> Sheila -> Byte -> IO Byte
read cyc Sheila{} a = do
  let _ = putStrLn $ show cyc ++ " -- " ++ "Sheila.read, " ++ show a
  case a of
    0x30 -> error $ "Sheila.read, " ++ show a
    -- other Sheila reads seen so far... return 0
    0x4E -> pure 0x0
    0x4F -> pure 0x0
    0x6C -> pure 0x0
    0x40 -> pure 0x0
    0xE0 -> pure 0x0
    _ -> error $ "Sheila.read, " ++ show a

write :: Cycles -> Sheila -> Byte -> Byte -> IO ()
write cyc Sheila{selectedRomRef} a b = do
  let _ = putStrLn $ show cyc ++ " -- " ++ "Sheila.write, " ++ show a ++ " = " ++ show b
  case a of
    0x30 -> writeIORef selectedRomRef b
    -- other Sheila write address seen so far... do nothing
    0x00 -> pure ()
    0x01 -> pure ()
    0x08 -> pure ()
    0x10 -> pure ()
    0x20 -> pure ()
    0x40 -> pure ()
    0x42 -> pure ()
    0x43 -> pure ()
    0x45 -> pure ()
    0x46 -> pure ()
    0x47 -> pure ()
    0x48 -> pure ()
    0x49 -> pure ()
    0x4B -> pure ()
    0x4C -> pure ()
    0x4D -> pure ()
    0x4E -> pure ()
    0x4F -> pure ()
    0x63 -> pure ()
    0x6C -> pure ()
    0x6D -> pure ()
    0x6E -> pure ()
    0xC0 -> pure ()
    0xE0 -> pure ()
    _ ->
      error $ "Sheila.write, " ++ show a ++ " = " ++ show b
