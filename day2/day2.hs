alpha = ['a'..'z']

testinput :: [String]
testinput = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz", "abcfe"]

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

matchOne :: String -> String -> Bool
matchOne x y = (== 1) . length . filter not $ zipWith (==) x y

excludeId :: String -> [String] -> [String]
excludeId s = filter (/= s)

similarities :: String -> String -> String
similarities x y = map fst . filter (\(a, b) -> a == b) $ zip x y

part2 :: [String] -> String
part2 (x:xs) = if hasMatch then similarities match x else part2 xs
  where match = head matches
        hasMatch = (length matches) == 1
        matches = filter (matchOne x) xs

main = do
  input <- readFile "input.txt"
  putStrLn "Part1: "
  print (part1 $ lines input)
  print (part2 $ lines input)
