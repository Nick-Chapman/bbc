
module Play(main) where -- first steps with emulation

import Control.Monad (when)

import Bbc.Addr (Addr(..),addAddr,addrOfHiLo)
import Bbc.MM as MM
import Bbc.Six502.Cycles (Cycles(..))
import Bbc.Six502.Decode (decode1)
import Bbc.Six502.Disassembler (displayOpAt)
import Bbc.Six502.Emu (six_stepInstruction)
import Bbc.Six502.Operations (Op)
import qualified Bbc.Mem as Mem (Effect(..))
import qualified Bbc.Six502.Cpu as Cpu (State(..),state0)

main :: MM -> IO ()
main mm = do
  let cyc0 :: Cycles = 0
  pc0 <- MM.run cyc0 mm (indirect (Addr 0xFFFC))
  let cpu0 :: Cpu.State = Cpu.state0 pc0
  loop cyc0 cpu0
  where
    loop :: Cycles -> Cpu.State -> IO ()
    loop cyc@Cycles{n} cpu@Cpu.State{pc} = do
      when (n `mod` 1000000 < 6) $ do
        op <- MM.run cyc mm (nextOp pc)
        putStrLn $ show cyc <> " -- " <> show cpu <> " -- " <> displayOpAt pc op
      (cpu,n) <- MM.run cyc mm (step cpu)
      cyc <- pure $ cyc + n
      loop cyc cpu

step :: Cpu.State -> Mem.Effect (Cpu.State, Cycles)
step = six_stepInstruction

indirect :: Addr -> Mem.Effect Addr
indirect a = do
  lo <- Mem.Read a
  hi <- Mem.Read (a+1)
  pure $ addrOfHiLo hi lo

nextOp :: Addr -> Mem.Effect Op
nextOp pc = do
  byte0 <- Mem.Read pc
  byte1 <- Mem.Read $ pc `addAddr` 1
  byte2 <- Mem.Read $ pc `addAddr` 2
  let bytes = [byte0,byte1,byte2]
  let op = decode1 bytes
  return op
