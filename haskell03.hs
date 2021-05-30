
add10toall :: [Int] -> [Int]
add10toall y = [x+10 | x <- y] 

multN :: Int -> [Int] -> [Int]
multN n y = [x*n | x <- y]

applyError :: [Int] -> [Int]
applyError y = [3*x+2 | x <- y]

addSuffix :: String -> [String] -> [String]
addSuffix str y = [x++str | x <- y]

selectgt5 :: [Int] -> [Int]
selectgt5 y = [x | x <- y, x > 5]

sumOdds :: [Int] -> Int
sumOdds y = sum[x | x <- y, mod x 2 /= 0]

selectExpr :: [Int] -> [Int]
selectExpr y = [x | x <- y, x >= 20, x <= 50, mod x 2 == 0]

countShorts :: [String] -> Int
countShorts y = length[x | x <- y, length x < 5]

--Erro
calcExpr :: [Float] -> [Float]
calcExpr y = [x^2 / 2 | x <- y, x > 10]

trSpaces :: String -> String
trSpaces y = concat[ if elem x " " then ['-'] else [x] | x <- y]
 
--selectSnd :: [(Int,Int)] -> [Int]
--selectSnd y = [(a,b) | a <- y, b <- y]

dotProd :: [Int] -> [Int] -> Int
dotProd a b = sum[ (x*y) | (x,y) <- zip a b]
