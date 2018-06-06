square x = x * x
fourth x = square (square x)

main = do
    print $ fourth 2