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

-- ("abcd", ["aecd", "abed", ...])
-- ("aecd", ["abcd", "abed", ...])
--
--
-- ("abcd", [])
-- ("aecd", ["adcd"])
--
--
-- "acd"

type Prototype = String

testinput :: [String]
testinput = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]

fabricsMatchPairs :: [Prototype] -> [(Prototype, [Prototype])]
fabricsMatchPairs fabrics = map (\fabric -> (fabric, filter (/= fabric) fabrics)) fabrics

-- findCandidates :: Prototype -> [Prototype] -> [Prototype]
-- findCandidates proto fabrics = filter (\(a, b) -> a /= b) $ map (\fabric -> zip proto fabric) fabrics
findCandidates proto fabrics =  filter (\pairs -> length pairs == 1) $ map go $ map (\fabric -> zip proto fabric) fabrics
  where
    go = filter (\(a, b) -> a /= b)

main = do
  input <- readFile "input.txt"
  putStrLn "Part1: "
  print (part1 $ lines input)
