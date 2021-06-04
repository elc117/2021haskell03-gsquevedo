--1
add10toall :: [Int] -> [Int]
add10toall y = [x+10 | x <- y] 

--2
multN :: Int -> [Int] -> [Int]
multN n y = [x*n | x <- y]

--3
multN' :: Int -> [Int] -> [Int]
multN' n y = map (\y -> y*n )y

--4
applyExpr :: [Int] -> [Int]
applyExpr y = [3*x+2 | x <- y]

--5
applyExpr' :: [Int] -> [Int]
applyExpr' x = map(\x -> 3*x+2)x

--6
addSuffix :: String -> [String] -> [String]
addSuffix str y = [x++str | x <- y]

--7
selectgt5 :: [Int] -> [Int]
selectgt5 y = [x | x <- y, x > 5]

--8
sumOdds :: [Int] -> Int
sumOdds y = sum[x | x <- y, mod x 2 /= 0]

--9
impar :: Int -> Int
impar x = if mod x 2 /= 0 then x else 0

sumOdds' :: [Int] -> Int
sumOdds' x = sum(map(\c -> impar c)x)

--10
selectExpr :: [Int] -> [Int]
selectExpr y = [x | x <- y, x >= 20, x <= 50, mod x 2 == 0]

--11
countShorts :: [String] -> Int
countShorts y = length[x | x <- y, length x < 5]

--12
calcExpr :: [Float] -> [Float]
calcExpr y = [x^2 / 2 | x <- y, x^2 / 2 > 10]

--13
trSpaces :: String -> String
trSpaces y = concat[ if elem x " " then ['-'] else [x] | x <- y]

--14
selectSnd :: [(Int,Int)] -> [Int]
selectSnd y = [ x | (_,x) <- y]

--15
dotProd :: [Int] -> [Int] -> Int
dotProd a b = sum[ (x*y) | (x,y) <- zip a b]
