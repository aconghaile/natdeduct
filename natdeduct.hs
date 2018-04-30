data Logical = And | Or | Implies

data Proposition = String | Proposition Logical Proposition

--parse :: [Char] -> Proposition
--parse x = if (noneSpecial x)
--		then Proposition x
--		else  

noneSpecial :: [Char] -> Bool
noneSpecial x = foldl (&&) True (map (not) (map (special) x))

special :: Char -> Bool
special x = x `elem` ['u','n','>']

bracket :: [Char] -> [[Char]]
bracket [] = []
bracket x = if (head x == '(')
	then split (')'==) [] (tail x)
	else [x]

split :: (Char -> Bool) -> [Char] -> [Char] -> [[Char]]
split c x [] = [x]
split c x y = if (c (head y))
	then [x , [head y], tail y]
	else split c (x ++ [(head y)]) (tail y)