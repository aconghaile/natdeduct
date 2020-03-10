data Logical = Conj | Disj | Imp
data Proposition = Atomic String | And Proposition Proposition | Or Proposition Proposition | Implies Proposition Proposition | Negation Proposition



instance Show Proposition where
	show (Atomic str) = str
	show (And a b) = "(" ++ (show a) ++ (show Conj) ++ (show b) ++ ")"
	show (Or a b) = "(" ++ (show a) ++ (show Disj) ++ (show b) ++ ")"
	show (Implies a b) = "(" ++ (show a) ++ (show Imp) ++ (show b) ++ ")"
	show (Negation a) = "(NOT (" ++ (show a) ++ "))"

instance Show Logical where
	show Conj = " AND "
	show Disj = " OR "
	show Imp = " IMPLIES "

parse :: String -> Proposition
parse [] = error "Empty Atomic"
parse ('(':xs) = combine (bracket 1 [] xs)
parse('!':xs) = negcombine (bracket 1 [] (tail xs))     
parse x = if (noneSpecial x)
		then Atomic x
		else combine (split special [] x) 

combine :: (String, String, String) -> Proposition
combine ([], [], [])  = error "Empty Logical"
combine (a, ")", []) = parse a
combine (a, ")", b) = combine (a, [head b], (tail b))
combine (a, x, b) = compound ((parse a), (toLogical x), (parse b))

negcombine :: (String, String, String) -> Proposition
negcombine ([], [], [])  = error "Empty Logical"
negcombine (a, ")", []) = Negation (parse a)
negcombine (a, ")", b) = negcombine (a, [head b], (tail b))
negcombine (a, x, b) = compound ((Negation (parse a)), (toLogical x), (parse b))

compound :: (Proposition, Logical, Proposition) -> Proposition
compound (a, Disj, b) = Or a b
compound (a , Conj, b) = And a b
compound (a, Imp, b) = Implies a b

toLogical :: String -> Logical
toLogical [] = error "Empty Logical"
toLogical "n" = Conj
toLogical "u" = Disj
toLogical ">" = Imp   

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

impfree :: Proposition -> Proposition
impfree (Implies a b) = Or (Negation (impfree a)) (impfree b)
impfree (And a b) = And (impfree a) (impfree b)
impfree (Or a b) = Or (impfree a) (impfree b)
impfree (Negation a) = Negation (impfree a)
impfree (Atomic a) = Atomic a

negfree :: Proposition -> Proposition
negfree (And a b) = And (negfree a) (negfree b)
negfree (Or a b) = Or (negfree a) (negfree b)
negfree (Negation (And a b)) = Or (negfree (Negation a)) (negfree (Negation b))
negfree (Negation (Or a b)) = And (negfree (Negation a)) (negfree (Negation b))
negfree (Negation (Negation (Atomic a))) = (Atomic a)
negfree p = p

disfree :: Proposition -> Proposition
disfree (And a b) = And (disfree a) (disfree b)
disfree (Or (And a b) c) = And (disfree (Or a c)) (disfree (Or b c))
disfree (Or c (And a b)) = And (disfree (Or c a)) (disfree (Or c b))
disfree p = p 

confree :: Proposition -> Proposition
confree (Or a b) = Or (confree a) (confree b)
confree (And (Or a b) c) = Or (confree (And a c)) (confree (And b c))
confree (And c (Or a b)) = Or (confree (And c a)) (confree (And c b))
confree p = p

consplit :: Proposition -> [Proposition]
consplit (And a b) = (consplit a) ++ (consplit b)
consplit p = [p] 

dissplit :: Proposition -> [Proposition]
dissplit (Or a b) = (dissplit a) ++ (dissplit b)
dissplit p = [p]

cnf = (disfree . negfree . impfree)
dnf = (confree . negfree . impfree)
conjuncts = (consplit . cnf)
disjuncts = (dissplit . dnf)
