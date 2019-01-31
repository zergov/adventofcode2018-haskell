alpha = ['a'..'z']

countChar :: Char -> String -> Int
countChar e = length . (filter (== e))

countChars :: String -> [Int]
countChars s = map (\ c -> countChar c s) alpha

findPairs = length . (filter (==2)) . countChars
findTrios = length . (filter (==3)) . countChars

part1 :: [String] -> Int
part1 ids = pairs * trios
  where
    pairs = length $ filter (>0) $ (map findPairs ids)
    trios = length $ filter (>0) $ (map findTrios ids)

main = do
  input <- getContents
  putStrLn "Part1: "
  print (part1 $ lines input)
