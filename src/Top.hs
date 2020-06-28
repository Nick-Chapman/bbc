
module Top(main) where

import System.Environment (getArgs)

import qualified Bbc.Rom as Rom
import qualified Play(main)

main :: IO ()
main = do
  args <- getArgs
  let conf = parseArgs args
  runConf conf

data RomKind = Basic | Mos deriving Show

data Mode
  = Dis RomKind
  | Play

data Conf = Conf
  { mode :: Mode
  }

defaultConf :: Conf
defaultConf = Conf
  { mode = Play
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
  Play -> Play.main
  Dis kind -> do
    let spec = specRom kind
    putStrLn $ "**disassemble... " ++ show (kind,spec)
    rom <- Rom.load spec
    Rom.disassemble rom

specRom :: RomKind -> Rom.Spec
specRom = \case
  Basic -> Rom.Spec { path = "roms/Basic2.rom", loadA = 0x8000, vectorExecA = [] }
  Mos -> Rom.Spec { path = "roms/Os12.rom", loadA = 0xC000
                  , vectorExecA = [0xFFFA, 0xFFFC, 0xFFFE] }
