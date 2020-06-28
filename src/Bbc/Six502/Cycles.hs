
module Bbc.Six502.Cycles (Cycles) where

import Text.Printf (printf)

newtype Cycles = Cycles { n :: Int } deriving (Eq,Ord,Num)

instance Show Cycles where show Cycles{n} = printf "%7d" n
