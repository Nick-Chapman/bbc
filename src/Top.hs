
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

data Mode
  = Disassemble6502

data Conf = Conf
  { mode :: Mode
  , verbose :: Bool
  , path :: String
  }

defaultConf :: Conf
defaultConf = Conf
  { mode = Disassemble6502
  , verbose = False
  , path = "roms/Basic2.rom"
  }

parseArgs :: [String] -> Conf
parseArgs args = loop args defaultConf where
  loop args conf = case args of
    [] -> conf
    "-v":rest -> loop rest $ conf { verbose = True }
    path:rest -> loop rest $ conf { path }

runConf :: Conf -> IO ()
runConf Conf{mode,path} = case mode of
  Disassemble6502 -> do
    putStrLn $ "**disassemble... " <> path
    rom <- Rom.load path
    disRom (disWhere path) rom

disWhere :: FilePath -> Addr
disWhere = \case
  "roms/Basic2.rom" -> 0x8000
  _ -> 0xC000

disRom :: Addr -> Rom -> IO ()
disRom addr prg = do
  let bytes = Rom.bytes prg
  let ops :: [Op] = decode bytes
  let bytes' = take (length bytes) $ reEncode ops -- in case 1 or 2 extra 0s
  when (bytes /= bytes') $ fail "re-assemble failed"
  mapM_ putStrLn $ displayOpLines addr ops
  return ()
