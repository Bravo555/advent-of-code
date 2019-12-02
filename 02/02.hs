import Computer
import Control.Monad

program = readProgram "input.txt"

run noun verb = fmap (runProgram noun verb)

solution = sequenceA [run noun verb program | noun <- [0..99], verb <- [0..99]]