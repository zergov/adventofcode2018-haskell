import Data.List
import Data.Maybe
import qualified Data.Set as Set

removePluses :: String -> String
removePluses = filter (not . (`elem` "+"))

parseInput :: String -> [Int]
parseInput = map (read::String->Int) . lines . removePluses

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = fromJust . (solve Set.empty) . (scanl (+) 0) . cycle
  where
    solve _ [] = Nothing
    solve seen (y:ys)
      | y `Set.member` seen = Just y
      | otherwise = solve (Set.insert y seen) ys

main = do
  input <- getContents
  putStrLn "Part1: "
  print $ part1 (parseInput input)
  putStrLn "Part2: "
  print $ part2 (parseInput input)
