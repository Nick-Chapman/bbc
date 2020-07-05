
module Bbc (Cycles,State,init,step,getScreen7) where

import Prelude hiding (init)

import Bbc.Addr (Addr(..),addrOfHiLo)
import Bbc.MM (MM(..))
import Bbc.Six502.Cycles (Cycles(..))
import Bbc.Six502.Emu (six_stepInstruction)
import Screen7 (Screen7)
import qualified Bbc.MM as MM (run)
import qualified Bbc.Mem as Mem (Effect(..))
import qualified Bbc.Ram as Ram (create)
import qualified Bbc.Rom as Rom (load,Spec(..))
import qualified Bbc.Sheila as Sheila (create)
import qualified Bbc.Six502.Cpu as Cpu (State(..),state0)
import qualified Screen7 (fromRam)

data State = State { mm :: MM, cyc :: Cycles, cpu :: Cpu.State }

getScreen7 :: State -> IO Screen7
getScreen7 State{mm=MM{ram}} = Screen7.fromRam ram

init :: IO State
init = do
  rom1 <- Rom.load (specRom Basic)
  rom2 <- Rom.load (specRom Mos)
  ram <- Ram.create
  sheila <- Sheila.create
  let mm = MM {rom1,rom2,ram,sheila}
  let cyc = 0
  pc <- MM.run cyc mm (indirect (Addr 0xFFFC))
  let cpu :: Cpu.State = Cpu.state0 pc
  return $ State { mm, cyc, cpu }

specRom :: RomKind -> Rom.Spec
specRom = \case
  Basic -> Rom.Spec { path = "roms/Basic2.rom", loadA = 0x8000, vectorExecA = [] }
  Mos -> Rom.Spec { path = "roms/Os12.rom", loadA = 0xC000
                  , vectorExecA = [0xFFFA, 0xFFFC, 0xFFFE] }

data RomKind = Basic | Mos deriving Show

step :: Cycles -> State -> IO State
step target State{mm,cyc=cyc0,cpu=cpu0} = do
  loop cyc0 cpu0
  where
    loop :: Cycles -> Cpu.State -> IO State
    loop cyc cpu = do
      if (cyc >= target) then return State {mm,cyc,cpu} else do
        (cpu,n) <- MM.run cyc mm (six_stepInstruction cpu)
        cyc <- pure $ cyc + n
        loop cyc cpu

indirect :: Addr -> Mem.Effect Addr
indirect a = do
  lo <- Mem.Read a
  hi <- Mem.Read (a+1)
  pure $ addrOfHiLo hi lo

