
module Bbc.Six502.Disassembler (displayOpLines) where

import Data.Char as Char
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Bbc.Addr (Addr,addAddr)
import Bbc.Byte (unByte,byteToSigned)
import Bbc.Six502.Decode (opBytes,opSize)
import Bbc.Six502.Operations (Op(..),Instruction(..),Mode(..),Arg(..))

displayOpLines :: Addr -> [Op] -> [String]
displayOpLines baseProgram ops = lines
  where
    lines = [ displayOpLine (reach a) a op | (a,op) <- indexedOps ]
    indexedOps = loop baseProgram ops
    loop a = \case
      [] -> []
      op:ops -> (a,op) : loop (a `addAddr` opSize op) ops
    reach a = a `elem` reachSet
    reachSet = reachable baseProgram indexedOps

displayOpLine :: Bool -> Addr -> Op -> String
displayOpLine reached addr op = line where

  line = withText (withCode base)

  base = show addr <> "  " <> showOpBytes op

  withCode s = if showCode then ljust 15 s <> asCode else s
  withText s = if showText then ljust 30 s <> asText else s

  showCode = reached || isOfficialOp
  showText = not reached && printable

  isOfficialOp = case op of Op i _ _ -> not (unofficial i); Unknown{} -> False

  asCode = case op of
    Unknown _ -> " ???"
    Op instruction mode rand ->
      (if unofficial instruction then "?" else " ")
      <> showInstruction instruction
      <> displayArg addr (mode,rand)

  asText = show bytesAsChars
  printable = all isPrintable bytesAsChars
  isPrintable c = Char.isAscii c && Char.isPrint c
  bytesAsChars = [Char.chr $ fromIntegral $ unByte byte | byte <- opBytes op]


reachable :: Addr -> [(Addr,Op)] -> Set Addr
reachable baseProgram indexedOps = loop Set.empty [baseProgram]
  where
    m :: Map Addr Op = Map.fromList indexedOps
    loop :: Set Addr -> [Addr] -> Set Addr
    loop seen = \case
      [] -> seen
      fringe -> do
        let new = filter (not . (`Set.member` seen)) fringe
        let seen' = seen `Set.union` Set.fromList new
        let next = new >>= step
        loop seen' next
    step :: Addr -> [Addr]
    step a = case Map.lookup a m of
      Nothing -> [] -- the address is outside the static code
      Just op -> nextPC a op

nextPC :: Addr -> Op -> [Addr]
nextPC a op = case op of
  Unknown _ -> []
  Op instr mode arg -> case instr of
    RTS -> []
    BRK -> [] -- assume there is no RTI from a BRK
    JMP -> dests
    JSR -> dests ++ [next]
    BCS -> branch
    BEQ -> branch
    BVS -> branch
    BMI -> branch
    BCC -> branch
    BNE -> branch
    BVC -> branch
    BPL -> branch
    _ -> [next]
    where
      branch = dests ++ [next]
      next = a `addAddr` opSize op
      dests = case (mode,arg) of
        (Absolute,ArgAddr a) -> [a]
        (Relative,ArgByte b) -> [a `addAddr` (2 + byteToSigned b)]
        (Indirect,_) -> []
        _ -> error $ "nextPC: " ++ show (instr,mode,arg)

showOpBytes :: Op -> String
showOpBytes op = unwords $ map show $ opBytes op

ljust :: Int -> String -> String
ljust n s = s <> take (max 0 (n - length s)) (repeat ' ')

showInstruction :: Instruction -> String
showInstruction = \case
  SBC_extra -> "SBC"
  instruction -> if unofficialNop instruction then "NOP" else show instruction

displayArg :: Addr -> (Mode,Arg) -> String
displayArg at = \case
  (Immediate,ArgByte b) -> " #&" <> show b
  (ZeroPage,ArgByte b) -> " &" <> show b
  (Relative,ArgByte b) -> " &" <> show (at `addAddr` (2 + byteToSigned b))
  (Absolute,ArgAddr a) -> " &" <> show a
  (Implied,ArgNull) -> ""
  (Accumulator,ArgNull) -> " A"
  (IndexedIndirect,ArgByte b) -> " (&" <> show b <> ",X)"
  (IndirectIndexed,ArgByte b) -> " (&" <> show b <> "),Y"
  (Indirect,ArgAddr a) -> " (&" <> show a <> ")"
  (ZeroPageX,ArgByte b) -> " &" <> show b <> ",X"
  (ZeroPageY,ArgByte b) -> " &" <> show b <> ",Y"
  (AbsoluteX,ArgAddr a) -> " &" <> show a <> ",X"
  (AbsoluteY,ArgAddr a) -> " &" <> show a <> ",Y"
  x -> error $ "displayArg: " <> show x

unofficial :: Instruction -> Bool
unofficial i = i `elem` [DCP,ISB,LAX,RLA,RRA,SAX,SLO,SRE,SBC_extra] || unofficialNop i

unofficialNop :: Instruction -> Bool
unofficialNop i = i `elem`
  [ NOP_04,NOP_44,NOP_64
  , NOP_0C
  , NOP_14,NOP_34,NOP_54,NOP_74,NOP_D4,NOP_F4
  , NOP_1A,NOP_3A,NOP_5A,NOP_7A,NOP_DA,NOP_FA
  , NOP_80
  , NOP_1C,NOP_3C,NOP_5C,NOP_7C,NOP_DC,NOP_FC
  ]
