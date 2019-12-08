{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read
import Data.Either (rights)
import Intcode

setFirstTwo :: Integer -> Integer -> MachineState -> MachineState
setFirstTwo first second state = setNth (setNth state 1 first) 2 second

runAndGetFirstCell :: Integer -> Integer -> MachineState -> Maybe Integer
runAndGetFirstCell first second cleanState =
    case execute (setFirstTwo first second cleanState) of
        Left _ -> Nothing
        Right state -> lookupCell state 0
    
main :: IO ()
main = do
    inputFileContents <- TIO.readFile "input.txt"
    let tokens = T.splitOn "," (T.strip inputFileContents)
    let instructions = map fst $ rights (map decimal tokens)
    -- before running the program, replace position 1 with the value 12 and replace position 2 with the value 2.
    let initialState = loadInitialState instructions
    let alteredState = setFirstTwo 12 2 initialState
    -- What value is left at position 0 after the program halts?
    print (execute alteredState)

    -- What combination of values at position 1 and 2 (between 0 and 99) will
    -- produce 19690720 at position 0?
    let inputs = sequence [[0..99], [0..99]]
    let results = map (\x -> (runAndGetFirstCell (x !! 0) (x !! 1) initialState, x)) inputs
    print (filter (\x -> fst x == Just 19690720) results)
