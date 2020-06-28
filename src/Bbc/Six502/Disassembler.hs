
module Bbc.Six502.Disassembler (displayOpLines,displayOpAt) where

import Data.Char as Char
import Data.Set (Set)
import Data.Map (Map)
import Text.Printf (printf)
import Data.List (sort,nub)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)

import Bbc.Addr (Addr,addAddr)
import Bbc.Byte (unByte,byteToSigned)
import Bbc.Six502.Decode (opBytes,opSize)
import Bbc.Six502.Operations (Op(..),Instruction(..),Mode(..),Arg(..))

displayOpAt :: Addr -> Op -> String
displayOpAt at op = base where
  base = ljust 18 (show at <> "   " <> showOpBytes op) <> asCode
  asCode = case op of
    Unknown _ -> " ???"
    Op instruction mode rand ->
      (if unofficial instruction then "?" else " ")
      <> showInstruction instruction
      <> displayArg show at (mode,rand)


displayOpLines :: Addr -> [Addr] -> [Op] -> [String]
displayOpLines loadA startAs ops = lines
  where
    lines = [ displayOpLine aMap (reach a) a op | (a,op) <- indexedOps ]
    indexedOps = loop loadA ops
    loop a = \case
      [] -> []
      op:ops -> (a,op) : loop (a `addAddr` opSize op) ops
    reach a = a `elem` reachSet
    reachSet = reachable startAs indexedOps
    destSet = [ dest | (a,op) <- indexedOps, dest <- branchDest a op, reach dest ]
    aMap :: Map Addr Lab = Map.fromList $ zip (sort $ nub $ destSet) (map Lab [1..])

newtype Lab = Lab Int
instance Show Lab where show (Lab i) = ".L" <> printf "%02d" i

branchDest :: Addr -> Op -> [Addr] -- branch or jump
branchDest at op = case op of
  Unknown _ -> []
  Op instr mode arg ->
    if isBranchInstruction instr then
      case (mode,arg) of
        (Absolute,ArgAddr a) -> [a]
        (Relative,ArgByte b) -> [at `addAddr` (2 + byteToSigned b)]
        (Indirect,_) -> []
        _ -> error $ "branchDest: " ++ show (instr,mode,arg)
    else []

isBranchInstruction :: Instruction -> Bool
isBranchInstruction = \case
  JMP -> t; JSR -> t; BCS -> t; BEQ -> t; BVS -> t; BMI -> t; BCC -> t; BNE -> t; BVC -> t; BPL -> t
  _ -> False
  where t = True


displayOpLine :: Map Addr Lab -> Bool -> Addr -> Op -> String
displayOpLine aMap reached at op = line where

  line = withText (withCode (withLabel base))

  base = show at <> "   " <> showOpBytes op

  withLabel s = if reached then ljust 18 s <> label else s
  withCode s = if showCode then ljust 24 s <> asCode else s
  withText s = if showText then ljust 44 s <> asText else s

  label = case Map.lookup at aMap of Just lab -> show lab; Nothing -> "."

  showCode = reached || isOfficialOp
  showText = not reached && printable

  isOfficialOp = case op of Op i _ _ -> not (unofficial i); Unknown{} -> False

  asCode = case op of
    Unknown _ -> " ???"
    Op instruction mode rand ->
      (if unofficial instruction then "?" else " ")
      <> showInstruction instruction
      <> displayArg showLabel at (mode,rand)

  showLabel a = case Map.lookup a aMap of
    Just lab -> show lab
    Nothing -> "&" <> show a

  asText = show bytesAsChars
  printable = all isPrintable bytesAsChars
  isPrintable c = Char.isAscii c && Char.isPrint c
  bytesAsChars = [Char.chr $ fromIntegral $ unByte byte | byte <- opBytes op]


reachable :: [Addr] -> [(Addr,Op)] -> Set Addr
reachable starts indexedOps = loop Set.empty starts
  where
    m :: Map Addr Op = Map.fromList indexedOps
    loop :: Set Addr -> [Addr] -> Set Addr
    loop seen = \case
      [] -> seen
      fringe -> do
        let new = [ a | a <- fringe, not (a `Set.member` seen), isJust $ Map.lookup a m ]
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

displayArg :: (Addr -> String) ->Addr -> (Mode,Arg) -> String
displayArg showLabel at = \case
  (Immediate,ArgByte b) -> " #&" <> show b
  (ZeroPage,ArgByte b) -> " &" <> show b
  (Relative,ArgByte b) -> " " <> showLabel (at `addAddr` (2 + byteToSigned b))
  (Absolute,ArgAddr a) -> " " <> showLabel a
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
