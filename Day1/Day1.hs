type Mass = Integer

-- Calculate the amount of fuel (in terms of mass) to carry a certain amount
-- of mass.
--
-- "take its mass, divide by three, round down, and subtract 2"
fuelNeededForMass :: Mass -> Mass
fuelNeededForMass mass = (mass `div` 3) - 2

-- Account for the fact that the fuel itself needs to be carried, with more
-- fuel!
fuelNeededToCarryMass :: Mass -> Mass
fuelNeededToCarryMass mass
    | baseFuelNeeded < 0  = 0
    | otherwise           = baseFuelNeeded + (fuelNeededToCarryMass baseFuelNeeded)
    where baseFuelNeeded = fuelNeededForMass mass

solutionPart1 :: [Mass] -> Integer
solutionPart1 = sum . (map fuelNeededForMass)

solutionPart2 :: [Mass] -> Integer
solutionPart2 = sum . (map fuelNeededToCarryMass)


main :: IO ()
main = do
    inputFileContents <- readFile "input.txt"
    let linesOfFiles = lines inputFileContents
    let masses = map (read :: String -> Integer) linesOfFiles
    print (solutionPart1 masses)
    print (solutionPart2 masses)
