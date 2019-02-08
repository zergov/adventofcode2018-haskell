alpha = ['a'..'z']

countChar :: Char -> String -> Int
countChar e = length . (filter (== e))

countChars :: String -> [Int]
countChars s = map (\ c -> countChar c s) alpha

findKChars k = length . (filter (==k)) . countChars

part1 :: [String] -> Int
part1 ids = pairs * trios
  where
    pairs = length . (filter (>0)) $ map (findKChars 2) ids
    trios = length . (filter (>0)) $ map (findKChars 3) ids

differsBy1 :: String -> String -> Bool
differsBy1 s1 s2 = ((length s1) - 1) == (length . filter comp $ zip s1 s2)
  where
    comp (a, b) = a == b

main = do
  input <- readFile "input.txt"
  putStrLn "Part1: "
  print (part1 $ lines input)
