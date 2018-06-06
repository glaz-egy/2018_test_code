--com n k 
--    | n == k    = 1
--    | k == 0    = 1
--    | otherwise = (n / k) * com n-1 k-1

no6 = take 10 [ans | ans <- [1..],ans `mod` 3 == 2, ans `mod` 5 == 1, ans `mod` 7 == 2]

no7 = take 1 [[ans..ans+3] | ans <- [1..],ans `mod` 7 == 0,(ans + 1) `mod` 9 == 0,(ans + 2) `mod` 11 == 0,(ans + 3) `mod` 13 == 0]

two_squares n = length[(a,b) | a <- [1..n],b <- [1..a], a^2 + b^2 == n]

extract_alpha s = [ans | ans <- s, ans `elem` "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"]

difference xs = [y - x | (x, y) <- zip xs (tail xs)]


-- Test answer

-- Test no1
loworup a 
       | a `elem` ['a'..'z']  = last (take (length ['a'..a]) ['A'..'Z'])
       | otherwise = last (take (length ['A'..a]) ['a'..'z'])

chenge_case c = [ loworup a | a <- c]

-- Test no2
pythagoras n = [(a, b, c) | c <- [1..n], b <- [1..c], a <- [1..b], gcd a b == 1 , a^2 + b^2 == c^2]

-- Test no3
second xs = last $ take 2 xs

third xs = last $ take 3 xs

-- Test no4
comb n k = product[n-k+1..n]/product[1..k]

comb_squares n = sum [ (comb n k)^2 | k <- [0..n]]

comb_hrlfsquares n = sum [ (comb n k)^2 | k <- [0..n], even k ]

