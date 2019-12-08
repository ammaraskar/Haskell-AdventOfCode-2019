module Intcode where
import qualified Data.Map as Map


data MachineState = MachineState {
    cells :: Map.Map Integer Integer,
    cursor :: Integer
    } deriving (Show, Eq)

cellsFromInts :: [Integer] -> Map.Map Integer Integer
cellsFromInts s = let listWithIdxs = zip [0..] s in
                Map.fromList listWithIdxs

loadInitialState :: [Integer] -> MachineState
loadInitialState s = MachineState (cellsFromInts s) 0

lookupCell :: MachineState -> Integer -> Maybe Integer
lookupCell state index = Map.lookup index (cells state)

setNth :: MachineState -> Integer -> Integer -> MachineState
setNth state index value = 
    MachineState (Map.insert index value (cells state)) (cursor state)

data ExecutionResult = Success MachineState 
                     | Halted MachineState
                     deriving (Show, Eq)

maybeWithError :: String -> Maybe a -> Either String a
maybeWithError error Nothing = Left error
maybeWithError _ (Just x) = Right x

get :: String -> Integer -> MachineState -> Either String Integer
get error idx state =
    maybeWithError error (lookupCell state idx)

getRelativeCursor :: String -> Integer -> MachineState -> Either String Integer
getRelativeCursor error offset state = get error ((cursor state) + offset) state

executeOneStep :: MachineState -> Either String ExecutionResult
executeOneStep state = do
    opcode <- getRelativeCursor "Cell not found under cursor" 0 state
    case opcode of
        1 -> do
            leftLoc <- getRelativeCursor "Operand 1 out of bounds" 1 state
            left <- get "Error getting operand 1" leftLoc state

            rightLoc <- getRelativeCursor "Operand 2 out of bounds" 2 state
            right <- get "Error getting operand 2" rightLoc state

            storageLoc <- getRelativeCursor "Operand 3 out of bounds" 3 state
            let newCells = Map.insert storageLoc (left + right) (cells state)
            Right $ Success (MachineState newCells ((cursor state) + 4))

        2 -> do
            leftLoc <- getRelativeCursor "Operand 1 out of bounds" 1 state
            left <- get "Error getting operand 1" leftLoc state

            rightLoc <- getRelativeCursor "Operand 2 out of bounds" 2 state
            right <- get "Error getting operand 2" rightLoc state

            storageLoc <- getRelativeCursor "Operand 3 out of bounds" 3 state
            let newCells = Map.insert storageLoc (left * right) (cells state)
            Right $ Success (MachineState newCells ((cursor state) + 4))

        99 -> Right $ Halted state

        unknown -> Left $ "Unknown opcode " ++ (show unknown)

execute :: MachineState -> Either String MachineState
execute state = case executeOneStep state of
    Right (Success state) -> execute state
    Right (Halted state) -> Right state
    Left error -> Left error