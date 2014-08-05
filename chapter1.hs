intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = x:[]
--doubleEveryOther (y:x:[]) = 2*y:x:[]
--doubleEveryOther (x:y:z:[]) = x:2*y:z:[]
doubleEveryOther (x:y:xs) = x:2*y:doubleEveryOther xs
--doubleEveryOther (x:xs) = reverse (x:xs)

doubleEveryOtherFromRight :: [Integer] -> [Integer]
doubleEveryOtherFromRight [] = []
doubleEveryOtherFromRight x = reverse(doubleEveryOther (reverse(x)))

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x 
	| x < 0 = []
	| otherwise  = (x `mod` 10) : toDigits (x `div` 10) 

toDigitsReverse :: Integer -> [Integer]
toDigitsReverse x = b_reverse (toDigits x) []

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (x `div` 10) + (x `mod` 10) + sumDigits xs

validate :: Integer -> Bool
--validate x = (sumDigits(doubleEveryOther (toDigitsReverse x)) + sumDigits(toDigitsReverse x)) `mod` 10
validate x = sumDigits(doubleEveryOtherFromRight (toDigitsReverse x)) `mod` 10 == 0

b_reverse :: [a] -> [a] -> [a]
b_reverse [] acc = acc
b_reverse (x:xs) acc = b_reverse xs (x:acc) 

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from to temp 
	| n <= 0 = []
	| otherwise = hanoi (n-1) from temp to ++ (from,to) : hanoi (n-1) temp to from

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg [Move]
	| n <= 0 = []

--hanoi x p1 p2 p3 
--	| x < 2 = []
--	| otherwise = moves (p1, [1..x]) (p2, []) (p3, []) []

--moves :: (Peg, [Integer]) -> (Peg,[Integer]) -> (Peg,[Integer]) -> [Move] -> [Move]
--moves (_, []) (_, []) (_, []) _ = error "Blah 1" 

--moves (a, x:xs) (b, []) (c, []) [] = moves (a, xs) (b, x:[]) (c, x:[]) ((a,b):[])

--moves (a, x:xs) (b, [y]) (c, []) [] = moves (a, xs) (b, []) (c, y:[]) ((b,c):[])

--moves (a, x:xs) (b, []) (c, [z]) acc = moves (a, xs) (b, [x]) (c, [z]) ((a,b):acc)

--moves (a, x:xs) (b, []) (c, z:zs) acc = moves (a, xs) (b, x:[]) (c, z:zs) ((a,b):acc)

--moves (a, []) (b, []) (c, z:zs) acc = acc

--moves (a, [x]) (b, [y]) (c, [z]) acc 
--	| y < z = moves (a, [x]) (b, []) (c, y:[z]) ((b,c):acc)
--	| z < y = moves (a, [x]) (b, z:[y]) (c, []) ((c,b):acc) 
--	| x < z = moves (a, []) (b, [y]) (c, x:[z]) ((a,c):acc)
--	| x < y = moves (a, []) (b, x:[y]) (c, [z]) ((a,b):acc)
--	| otherwise = error "Blah 2"

--moves (a, x:xs) (b, y:ys) (c, []) acc = moves (a, xs) (b, y:ys) (c, x:[]) ((a,c):acc)

--moves (a, []) (b, y:ys) (c, z:zs) acc
--	| z < y = moves (a, []) (b, z:y:ys) (c, zs) ((c,b):acc)
--	| otherwise = moves (a, []) (b, ys) (c, y:z:zs) ((b,c):acc)

--moves (a, x:xs) (b, y:ys) (c, [z]) acc 
--	| x < y = moves (a, xs) (b, x:y:ys) (c, [z]) ((a,b):acc)
--	| y < z = moves (a, x:xs) (b, ys) (c, y:[z]) ((b,c):acc)
--	| x < z = moves (a, xs) (b, y:ys) (c, x:[z]) ((a,c):acc)
--	| z < y = moves (a, x:xs) (b, z:y:ys) (c, []) ((c,b):acc)

--moves (a, x:xs) (b, y:ys) (c, z:zs) acc 
--	| x < y = moves (a, xs) (b, x:y:ys) (c, z:zs) ((a,b):acc)
--	| y < z = moves (a, x:xs) (b, ys) (c, y:z:zs) ((b,c):acc)
--	| x < z = moves (a, xs) (b, y:ys) (c, x:z:zs) ((a,c):acc)
--	| z < y = moves (a, x:xs) (b, z:y:ys) (c, zs) ((c,b):acc)
--	| otherwise = error "Blah 3"

--moves (a, []) (b, y:ys) (c, []) acc = acc
