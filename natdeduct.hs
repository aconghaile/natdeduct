data Logical = And | Or | Implies

data Proposition = Atomic String | Compound Proposition Logical Proposition

instance Show Proposition where
	show (Atomic str) = str
	show (Compound a log b) = "(" ++ (show a) ++ (show log) ++ (show b) ++ ")"

instance Show Logical where
	show And = " AND "
	show Or = " OR "
	show Implies = " IMPLIES "

parse :: String -> Proposition
parse [] = error "Empty Atomic"
parse ('(':xs) = combine (bracket 1 [] xs)     
parse x = if (noneSpecial x)
		then Atomic x
		else combine (split special [] x) 

combine :: (String, String, String) -> Proposition
combine ([], [], [])  = error "Empty Logical"
combine (a, ")", []) = parse a
combine (a, ")", b) = combine (a, [head b], (tail b))
combine (a, x, b) = Compound (parse a) (toLogical x) (parse b)



toLogical :: String -> Logical
toLogical [] = error "Empty Logical"
toLogical "n" = And
toLogical "u" = Or
toLogical ">" = Implies   

noneSpecial :: String -> Bool
noneSpecial x = foldl (&&) True (map (not) (map (special) x))

special :: Char -> Bool
special x = x `elem` ['u','n','>', ')']

bracket :: Integer -> String -> String -> (String, String, String) 
bracket _ _ []  = error "Mismatched Brackets"
bracket z x ('(':ys) = bracket (z+1) (x++"(") (ys)
bracket 1 x (')':ys) = (x, ")", ys)
bracket z x (')':ys) = bracket (z-1) (x++")") (ys)
bracket z x (y:ys) = bracket z (x++[y]) ys

split :: (Char -> Bool) -> String -> String -> (String, String, String)
split c x [] = ("", x, "")
split c x y = if (c (head y))
	then (x , [head y], tail y)
	else split c (x ++ [(head y)]) (tail y)
