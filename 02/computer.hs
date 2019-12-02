module Computer
( readProgram
, runProgram
, readMemory
) where

import Data.List.Split


type Program = [Int]
type InstrPtr = Int
type Addr = Int

data Args = Args { src1 :: Addr
                 , src2 :: Addr
                 , dst :: Addr
                 } deriving (Show, Eq)

data Instruction = Add Args | Multiply Args | Halt deriving (Show, Eq)

runProgram :: Int -> Int -> Program -> (InstrPtr, Program)
runProgram noun verb program = stepProgram (0, (modifyMemory 01 noun . modifyMemory 02 verb) program)


parseInstr :: Program -> Instruction
parseInstr (1:rest) = Add (parseArgs rest)
    where parseArgs (src1:src2:dst:rest) = Args src1 src2 dst
parseInstr (2:rest) = Multiply (parseArgs rest)
    where parseArgs (src1:src2:dst:rest) = Args src1 src2 dst
parseInstr (99:_) = Halt
parseInstr (opcode:rest) = error ("invalid instruction: " ++ show opcode)

readProgram :: FilePath -> IO Program
readProgram path = fmap (map read . splitOn ",") (readFile path)

stepProgram :: (InstrPtr, Program) -> (InstrPtr, Program)
stepProgram (ip, program)
    | parseInstr (drop ip program) /= Halt = stepProgram ((ip+4), execInstr (parseInstr (drop ip program)) program)
    | otherwise = (ip, program)

execInstr :: Instruction -> Program -> Program
execInstr Halt program = program
execInstr (Add args) program = modifyMemory (dst args) ((readMemory (src1 args) program) + (readMemory (src2 args) program)) program
execInstr (Multiply args) program = modifyMemory (dst args) ((readMemory (src1 args) program) * (readMemory (src2 args) program)) program

modifyMemory :: Addr -> Int -> Program -> Program
modifyMemory addr val program = take addr program ++ [val] ++ drop (addr + 1) program

readMemory :: Addr -> Program -> Int
readMemory addr program = program!!addr
