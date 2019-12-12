-- {-# LANGUAGE NamedFieldPuns #-}

module Computer
( readProgram
, runProgram
, runProgram2
, readMemory
) where

import Data.List.Split

type Program = [Int]
type InstrPtr = Int
type Addr = Int

data Param = Immediate { unParam :: Int }
           | Position { unParam :: Int } deriving (Show, Eq)

data Instruction = Add Param Param Param
                 | Mul Param Param Param
                 | Input Param
                 | Output Param
                 | JumpIfTrue Param Param
                 | JumpIfFalse Param Param
                 | LessThan Param Param Param
                 | Equals Param Param Param
                 | Halt deriving (Show, Eq)

data Machine = Machine {
    ip :: Int,
    program :: Program,
    input :: [Int],
    output :: [Int],
    running :: Bool
} deriving (Show, Eq)

defaultMachine = Machine {
    ip = 0,
    program = [],
    input = [],
    output = [],
    running = True
}

prog :: [Int]
prog = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,68,5,225,1101,71,12,225,1,117,166,224,1001,224,-100,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1001,66,36,224,101,-87,224,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1101,26,51,225,1102,11,61,224,1001,224,-671,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,59,77,224,101,-136,224,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,1101,11,36,225,1102,31,16,225,102,24,217,224,1001,224,-1656,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,101,60,169,224,1001,224,-147,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,38,69,225,1101,87,42,225,2,17,14,224,101,-355,224,224,4,224,102,8,223,223,1001,224,2,224,1,224,223,223,1002,113,89,224,101,-979,224,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1102,69,59,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,1007,226,226,224,1002,223,2,223,1006,224,344,1001,223,1,223,1108,226,677,224,102,2,223,223,1005,224,359,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,404,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,434,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,449,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,464,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,479,101,1,223,223,1007,226,677,224,102,2,223,223,1006,224,494,101,1,223,223,107,677,677,224,102,2,223,223,1005,224,509,101,1,223,223,108,677,677,224,102,2,223,223,1006,224,524,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,539,101,1,223,223,107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,569,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,584,1001,223,1,223,1108,226,226,224,102,2,223,223,1005,224,599,1001,223,1,223,1107,677,677,224,1002,223,2,223,1006,224,614,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,629,1001,223,1,223,108,226,226,224,102,2,223,223,1005,224,644,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,659,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226]

runProgramNounVerb :: Int -> Int -> Program -> Machine
runProgramNounVerb noun verb program = stepProgram ((modifyMemory 01 noun . modifyMemory 02 verb) defaultMachine { program = program })

runProgram :: [Machine] -> [Machine]
runProgram machines
    | running (head machines) == True = runProgram ((stepProgram $ head machines):machines)
    | otherwise = machines

runProgram2 :: Program -> Machine
runProgram2 program = stepProgram (defaultMachine { program = program })

runProgramWithInput :: Program -> [Int] -> [Machine]
runProgramWithInput prog input = runProgram [(defaultMachine {program=prog, input=input})]

parseOpcode :: InstrPtr -> Int
parseOpcode = (`mod` 100)

parseParam :: Int -> InstrPtr -> Int -> Param
parseParam pos instr
    | mode == 0 = Position
    | mode == 1 = Immediate
    | otherwise = error ("Incorrect parameter mode on position " ++ show pos ++ ": " ++ show instr)
    where mode = instr `div` 10^(pos+2) `mod` 10

getParam :: Param -> Machine -> Int
getParam (Immediate a) _ = a
getParam (Position a) prog = readMemory a prog

makeOp1 :: Program -> (Param -> Instruction) -> (Instruction, Int)
makeOp1 (opcode:arg:_) op = (op (parseParam 0 opcode arg), 2)

makeOp2 :: Program -> (Param -> Param -> Instruction) -> (Instruction, Int)
makeOp2 (opcode:arg1:arg2:_) op = (op (argn 0 arg1) (argn 1 arg2), 3)
    where argn n = parseParam n opcode

makeOp3 :: Program -> (Param -> Param -> Param -> Instruction) -> (Instruction, Int)
makeOp3 (opcode:arg1:arg2:arg3:_) op = (op (argn 0 arg1) (argn 1 arg2) (argn 2 arg3), 4)
    where argn n = parseParam n opcode

parseInstr :: Program -> (Instruction, Int)
parseInstr (op:rest)
    | parseOpcode op == 1 = makeOp3 (op:rest) Add
    | parseOpcode op == 2 = makeOp3 (op:rest) Mul
    | parseOpcode op == 3 = makeOp1 (op:rest) Input
    | parseOpcode op == 4 = makeOp1 (op:rest) Output
    | parseOpcode op == 5 = makeOp2 (op:rest) JumpIfTrue
    | parseOpcode op == 6 = makeOp2 (op:rest) JumpIfFalse
    | parseOpcode op == 7 = makeOp3 (op:rest) LessThan
    | parseOpcode op == 8 = makeOp3 (op:rest) Equals
    | parseOpcode op == 99 = (Halt, 1)
    | otherwise = error ("invalid instruction (" ++ show op ++ "): " ++ show (op:rest))


readProgram :: FilePath -> IO Program
readProgram path = fmap (map read . splitOn ",") (readFile path)

stepProgram :: Machine -> Machine
stepProgram machine = let newmachine = (execInstr (fst parsedInstr) machine) in newmachine {ip = ip newmachine + snd parsedInstr}
    where parsedInstr = parseInstr (drop (ip machine) (program machine))

jump :: InstrPtr -> Machine -> Machine
jump ip machine = machine { ip = ip - 3 } -- subtract 3 because its added back at `stepInstr` so its correct

execInstr :: Instruction -> Machine -> Machine
execInstr Halt machine = machine { running = False }
execInstr (Add arg1 arg2 arg3) machine = modifyMemory (unParam arg3) ((getParam arg1 machine) + (getParam arg2 machine)) machine
execInstr (Mul arg1 arg2 arg3) machine = modifyMemory (unParam arg3) ((getParam arg1 machine) * (getParam arg2 machine)) machine
execInstr (Input arg) machine = modifyMemory (unParam arg) (head $ input machine) machine {input = tail $ input machine }
execInstr (Output arg) machine = machine {output = (getParam arg machine):(output machine)}
execInstr (JumpIfTrue arg1 arg2) machine = if (getParam arg1 machine) /= 0 then jump (getParam arg2 machine) machine else machine 
execInstr (JumpIfFalse arg1 arg2) machine = if (getParam arg1 machine) == 0 then jump (getParam arg2 machine) machine else machine
execInstr (LessThan arg1 arg2 arg3) machine = modifyMemory (unParam arg3) (if (getParam arg1 machine) < (getParam arg2 machine) then 1 else 0) machine
execInstr (Equals arg1 arg2 arg3) machine = modifyMemory (unParam arg3) (if (getParam arg1 machine) == (getParam arg2 machine) then 1 else 0) machine

modifyMemory :: Addr -> Int -> Machine -> Machine
modifyMemory addr val machine = machine { program = take addr (program machine) ++ [val] ++ drop (addr + 1) (program machine)}

readMemory :: Addr -> Machine -> Int
readMemory addr machine = (program machine)!!addr
