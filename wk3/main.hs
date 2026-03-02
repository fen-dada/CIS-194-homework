skips :: [a] -> [[a]]
skips xs = map get [1 .. length xs]
  where
    index = zip xs [1 .. length xs]
    get n = map fst (filter (is n) index)
    is n (x, i) = mod i n == 0

localMax :: [Integer] -> [Integer]
localMax (a : b : c : xs)
  | a < b && b < c = b : localMax xs
  | otherwise = localMax (c : xs)
localMax _ = []

countAll :: [Integer] -> [Int]
countAll xs = map count [0 .. 9]
  where
    count x = length (filter (== x) xs)

histogram :: [Integer] -> String
histogram xs = unlines (gen rows counts) ++ "==========\n0123456789\n"
  where
    counts = countAll xs
    rows = maximum counts
    gen h cts = map (getRow cts) [h, h - 1 .. 1]
    getRow cts h = map (marker h) cts
    marker h c = if c >= h then '*' else ' '
