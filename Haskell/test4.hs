collatz x
    | even x    = div x 2
    | otherwise = x * 3 + 1

main = do
    print $ collatz 2