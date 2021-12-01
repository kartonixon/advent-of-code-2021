readIntegers :: FilePath -> IO [Integer]
readIntegers path = do
    contents <- readFile path
    let integers = map (read :: String -> Integer) . lines $ contents
    return integers

compareTwoInts :: (Integer, Integer) -> Integer
compareTwoInts (x, y)   | x < y = 1
                        | otherwise = 0

tripletSum :: (Integer, Integer, Integer) -> Integer 
tripletSum (x, y, z) = x + y + z

calculatePart1 :: [Integer] -> Integer
calculatePart1 ints = do
    sum (zipWith (curry compareTwoInts) ints (tail ints))

calculatePart2 :: [Integer] -> Integer 
calculatePart2 ints = do
    calculatePart1 (map tripletSum (zip3 ints (tail ints) (tail (tail ints))))
    
main :: IO ()
main = do
    input <- readIntegers "day1-input.txt"
    print (calculatePart1 input)
    print (calculatePart2 input)