import Computer
import Control.Monad

solution program = filter (\x -> (readMemory 0 x) == 19690720) [snd $ runProgram noun verb program | noun <- [0..99], verb <- [0..99]]

main = do
    program <- readProgram "input.txt"
    putStrLn $ show $ solution program
