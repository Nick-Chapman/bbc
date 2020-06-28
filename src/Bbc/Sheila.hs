
module Bbc.Sheila (Sheila,create,read,write) where

import Prelude hiding (read)

import Bbc.Byte as Byte
import Bbc.Six502.Cycles (Cycles)

data Sheila = Sheila -- what wll go here?

create :: IO Sheila
create = do
  return Sheila

read :: Cycles -> Sheila -> Byte -> IO Byte
read cyc Sheila a = do
  let b :: Byte = 0x0
  putStrLn $ show cyc ++ " -- " ++ "Sheila.read, " ++ show a ++ " -> " ++ show b
  return b

write :: Cycles -> Sheila -> Byte -> Byte -> IO ()
write cyc Sheila a b = do
  let _ = putStrLn $ show cyc ++ " -- " ++ "Sheila.write, " ++ show a ++ " = " ++ show b
  return ()
