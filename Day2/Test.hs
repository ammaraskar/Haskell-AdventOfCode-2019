import qualified Data.Map as Map
import Test.Hspec
import Test.QuickCheck
import Intcode

testAdd :: Integer -> Integer -> Integer -> Bool
testAdd op1 op2 location = do
    let result = executeOneStep (loadInitialState [1, 4, 5, location, op1, op2])
    case result of
        Right (Intcode.Success state) -> 
                Map.lookup location (cells state) == Just (op1 + op2)
        _ -> False

testMult :: Integer -> Integer -> Integer -> Bool
testMult op1 op2 location = do
    let result = executeOneStep (loadInitialState [2, 4, 5, location, op1, op2])
    case result of
        Right (Intcode.Success state) -> 
                Map.lookup location (cells state) == Just (op1 * op2)
        _ -> False

main :: IO ()
main = hspec $ do
    describe "Intcode.cellsFromInts" $ do
        it "returns the empty map for empty list" $ do
            cellsFromInts [] `shouldBe` Map.empty

    describe "Intcode.executeOneStep" $ do
        it "errors out when there is no opcode" $ do
            executeOneStep (loadInitialState []) 
                `shouldBe` Left "Cell not found under cursor"

        it "errors out with unknown opcodes" $ do
            executeOneStep (loadInitialState [13])
                `shouldBe` Left "Unknown opcode 13"

        it "errors out when there aren't enough operands" $ do
            executeOneStep (loadInitialState [1]) 
                `shouldBe` Left "Operand 1 out of bounds"
            executeOneStep (loadInitialState [1, 0]) 
                `shouldBe` Left "Operand 2 out of bounds"
            executeOneStep (loadInitialState [1, 0, 0]) 
                `shouldBe` Left "Operand 3 out of bounds"

        it "errors out when the operands are out of bounds" $ do
            executeOneStep (loadInitialState [1, 99, 0, 0])
                `shouldBe` Left "Error getting operand 1"
            executeOneStep (loadInitialState [1, 0, 99, 0])
                `shouldBe` Left "Error getting operand 2"

        it "handles the add opcode" $ property $ testAdd
        it "handles the multiplication opcode" $ property $ testMult

        it "handles the halt opcode" $ do
            let initialState = loadInitialState [99]
            executeOneStep initialState `shouldBe` Right (Halted initialState)

    describe "Intcode.execute" $ do
        it "works for the first example " $ do
            execute (loadInitialState [1, 0, 0, 0, 99]) `shouldBe`
                Right (MachineState (cellsFromInts [2, 0, 0, 0, 99]) 4)
        it "works for the second example" $ do
            execute (loadInitialState [2,3,0,3,99]) `shouldBe`
                Right (MachineState (cellsFromInts [2,3,0,6,99]) 4)
        it "works for the third example" $ do
            execute (loadInitialState [2,4,4,5,99,0]) `shouldBe`
                Right (MachineState (cellsFromInts [2,4,4,5,99,9801]) 4)
        it "works for the fourth example" $ do
            execute (loadInitialState [1,1,1,4,99,5,6,0,99]) `shouldBe`
                Right (MachineState (cellsFromInts [30,1,1,4,2,5,6,0,99]) 8)
