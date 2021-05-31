
add10toall :: [Int] -> [Int]
add10toall y = [x+10 | x <- y] 

multN :: Int -> [Int] -> [Int]
multN n y = [x*n | x <- y]

multN' :: Int -> [Int] -> [Int]
multN' n y = map (\y -> y*n )y

applyExpr :: [Int] -> [Int]
applyExpr y = [3*x+2 | x <- y]

applyExpr' :: [Int] -> [Int]
applyExpr' x = map(\x -> 3*x+2)x

applyError :: [Int] -> [Int]
applyError y = [3*x+2 | x <- y]

addSuffix :: String -> [String] -> [String]
addSuffix str y = [x++str | x <- y]

selectgt5 :: [Int] -> [Int]
selectgt5 y = [x | x <- y, x > 5]

sumOdds :: [Int] -> Int
sumOdds y = sum[x | x <- y, mod x 2 /= 0]

impar :: Int -> Int
impar x = if mod x 2 /= 0 then x else 0

sumOdds' :: [Int] -> Int
sumOdds' x = sum(map(\c -> impar c)x)

selectExpr :: [Int] -> [Int]
selectExpr y = [x | x <- y, x >= 20, x <= 50, mod x 2 == 0]

countShorts :: [String] -> Int
countShorts y = length[x | x <- y, length x < 5]

calcExpr :: [Float] -> [Float]
calcExpr y = [x^2 / 2 | x <- y, x^2 / 2 > 10]

trSpaces :: String -> String
trSpaces y = concat[ if elem x " " then ['-'] else [x] | x <- y]
 
selectSnd :: [(Int,Int)] -> [Int]
selectSnd y = [ x | (_,x) <- y]

dotProd :: [Int] -> [Int] -> Int
dotProd a b = sum[ (x*y) | (x,y) <- zip a b]
