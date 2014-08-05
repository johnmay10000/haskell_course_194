hanoi x p1 p2 p3 = doMoves (p1,[1..x]) (p2, []) (p3, []) []

--doMoves (_, []) (b,[]) (c,[]) acc = acc
--doMoves (a, x:xs) (b,y:ys) (c,z:zs) acc 
	--| x < y = doMoves (a, x:xs) (b,y:ys) (c,z:zs)
	--| x < z = doMoves (a, x:xs) (b,y:ys) (c,z:zs)
	--| z < y = doMoves (a, x:xs) (b,y:ys) (c,z:zs)
	--| otherwise doMoves (a, x:xs) (b,y:ys) (c,z:zs)
--doMove :: (Peg,Integer) -> (Peg,Integer) -> Move
--doMove (a,x) (b,y) = ()

moves (_, []) (_, x:xs) (_, y:ys) [] = []

move :: (Peg, [Integer]) -> (Peg,[Integer]) -> Move
move 


moves :: (Peg, [Integer]) -> (Peg,[Integer]) -> (Peg,[Integer]) -> [Move] -> [Move]


moves (_, []) (_, []) (_, []) [] = []
moves (_, []) (_, []) (_, []) acc = []
moves (_, []) (_, x:xs) (_, y:ys) [] = []
moves (_, []) (_, x:xs) (_, y:ys) acc = []
moves (_, z:zs) (_, []) (_, y:ys) [] = []
moves (_, z:zs) (_, []) (_, y:ys) acc = []
moves (_, []) (_, []) (_, y:ys) [] = []
moves (_, []) (_, []) (_, y:ys) acc = []
moves (_, []) (_, x:xs) (_, []) [] = []
moves (_, []) (_, x:xs) (_, []) acc = acc
moves (a, x:xs) (b, y:ys) (c, z:zs) acc 
	| x < y = moves (a, xs) (b, x:y:ys) (c, z:zs) ((a,b):acc)
	| x < z = moves (a, xs) (b, y:ys) (c, x:z:zs) ((a,c):acc)
	| z < y = moves (a, x:xs) (b, z:y:ys) (c, zs) ((c,b):acc)
	| z < x = moves (a, z:x:xs) (b, y:ys) (c, zs) ((c,a):acc)


moves (a, x:xs) (b, y:ys) (c, []) acc 
	| x < y = moves (a, xs) (b, x:y:ys) (c, []) ((a,b):acc)
	| otherwise = moves (a, xs) (b, y:ys) (c, x:[]) ((a,c):acc)

moves (a, x:xs) (b, []) (c, z:zs) acc
	|


	| otherwise = moves (a, z:[]) (b, y:ys) (c, zs) ((c,a):acc)

		| otherwise = moves (a, xs) (b, y:ys) (c, x:[]) ((a,c):acc)

moves (a, []) (b, y:ys) (c, z:zs) acc 
	| z < y = moves (a, []) (b, z:y:ys) (c, zs) ((c,b):acc)
	| otherwise = moves (a, []) (b, ys) (c, y:z:zs) ((b,c):acc)



	| x < y = moves (a, xs) (b, x:y:ys) (c, z:zs) ((a,b):acc)
	| x < z = moves (a, xs) (b, y:ys) (c, x:z:zs) ((a,c):acc)
	| z < y = moves (a, x:xs) (b, z:y:ys) (c, zs) ((c,b):acc)
	| otherwise = error "Blah 3"



	| x < y = moves (a, []) (b, x:[y]) (c, [z]) ((a,b):acc)
	| x < z = moves (a, []) (b, [y]) (c, x:[z]) ((a,c):acc)
	| z < y = moves (a, [x]) (b, z:[y]) (c, []) ((c,b):acc)
	| otherwise = error "Blah 2"

	moves (a, x:xs) (b, ys) (c, y:[]) ((b,c):acc)