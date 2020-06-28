
module Bbc.Sheila (Sheila,create,read,write) where

import Prelude hiding (read)

import Bbc.Byte as Byte

data Sheila = Sheila -- what wll go here?

create :: IO Sheila
create = do
  return Sheila

read :: Sheila -> Byte -> IO Byte
read Sheila a = do
  let b :: Byte = 0x0
  putStrLn $ "Sheila.read, " ++ show a ++ " -> " ++ show b
  undefined
  -- return b

write :: Sheila -> Byte -> Byte -> IO ()
write Sheila a b = do
  putStrLn $ "Sheila.write, " ++ show a ++ " = " ++ show b
