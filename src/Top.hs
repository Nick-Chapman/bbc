
module Top(main) where

import System.Environment (getArgs)
import Control.Monad (when)

import Bbc.Addr (Addr)
import Bbc.Rom (Rom)
import Bbc.Six502.Decode (decode,reEncode)
import Bbc.Six502.Disassembler (displayOpLines)
import Bbc.Six502.Operations(Op)
import qualified Bbc.Rom as Rom

main :: IO ()
main = do
  args <- getArgs
  let conf = parseArgs args
  runConf conf

data RomKind = Basic | Mos deriving Show

data Mode
  = Dis RomKind

data Conf = Conf
  { mode :: Mode
  }

defaultConf :: Conf
defaultConf = Conf
  { mode = Dis Basic
  }

parseArgs :: [String] -> Conf
parseArgs args = loop args defaultConf where
  loop args conf = case args of
    [] -> conf
    "--dis-basic":rest -> loop rest $ conf { mode = Dis Basic }
    "--dis-mos":rest -> loop rest $ conf { mode = Dis Mos }
    _path:_rest -> error $ "Unexpected command-line args: " ++ show args --loop rest $ conf { path }

runConf :: Conf -> IO ()
runConf Conf{mode} = case mode of
  Dis kind -> do
    let spec@SpecRom{path,loadA} = specRom kind
    putStrLn $ "**disassemble... " ++ show (kind,spec)
    rom <- Rom.load path
    disRom loadA rom

data SpecRom = SpecRom { path :: FilePath, loadA :: Addr } deriving Show

specRom :: RomKind -> SpecRom
specRom = \case
  Basic -> SpecRom { path = "roms/Basic2.rom", loadA = 0x8000 }
  Mos -> SpecRom { path = "roms/Os12.rom", loadA = 0xC000 }

disRom :: Addr -> Rom -> IO ()
disRom addr prg = do
  let bytes = Rom.bytes prg
  let ops :: [Op] = decode bytes
  let bytes' = take (length bytes) $ reEncode ops -- in case 1 or 2 extra 0s
  when (bytes /= bytes') $ fail "re-assemble failed"
  mapM_ putStrLn $ displayOpLines addr ops
  return ()
