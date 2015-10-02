-- type Variable = String

data Proposition = Constant Bool
				 | Variable String
				 | Negation Proposition 
				 | Conjunction Proposition Proposition 
				 | Disjunction Proposition Proposition 
				 | Implication Proposition Proposition
				 deriving (Show)

type Environment = [(String,Bool)]

find :: Environment -> String -> Maybe Bool
find [] _ = Nothing
find ((x,y):l) k
	| x == k 	= Just y
	| otherwise 	= find l k

addOrReplace :: Environment -> String -> Bool -> Environment
addOrReplace l x b = addAux l [] x b
	where 	addAux [] l x b = (x,b):reverse l
		addAux (t@(l,m):n) q x b 
			| l == x 	= reverse ((x,b):q) ++ n
			| otherwise	= addAux n (t:q) x b

remove :: Environment -> String -> Environment
remove e k = remAux e k []
	where 	remAux [] _ l = reverse l
		remAux ((x,y):m) k l 
			| x==k 		= remAux m k l
			| otherwise	= remAux m k ((x,y):l)

evalP :: Environment -> Proposition -> Maybe Bool

evalP _ (Constant b) 	  = Just b
evalP e (Variable s)      = find e s 
evalP e (Negation p)      = f (evalP e p)	
							where 
								f (Just p) = Just (not p)
								f _ 	   = Nothing
evalP e (Conjunction p q) = f (evalP e p) (evalP e q)
							where
								f (Just p) (Just q) = Just (p && q)
								f _ _ 				= Nothing
evalP e (Disjunction p q) = f (evalP e p) (evalP e q)
							where
								f (Just p) (Just q) = Just (p || q)
								f _ _ 				= Nothing

evalP e (Implication p q) = f (evalP e p) (evalP e q)
							where 
								f Nothing _ 		       = Nothing
								f _ Nothing 		       = Nothing
								f (Just True) (Just False) = Just False
								f _ _				       = Just True



vars :: Proposition -> [String]

--vars (Constant b) 		= []
--vars (Variable s)		= s : []
--vars (Negation p) 		= --vars p 
--vars (Conjunction p q) 	= --vars p ++ --vars q
--vars (Disjunction p q)  = --vars p ++ --vars q
--vars (Implication p q)  = vars p ++ vars q

vars p = f p []
	where
		f (Constant b) xs 		= xs
		f (Variable s) xs 		= if elem s xs then xs else s : xs
		f (Negation p) xs 		= f p xs
		f (Conjunction p q) xs  = f p (f q xs)
		f (Disjunction p q) xs  = f p (f q xs)
		f (Implication p q) xs  = f p (f q xs)

{-
generaPeor :: Proposition -> ( Environment , Environment ) -> ( Environment , Environment )

generaPeor (Constant _) (xs,ys) = (xs,ys)
generaPeor (Negation (Variable x)) (xs,ys) = if not $ elem x (map fst xs) then ((x,True):xs,ys)
												else if (snd $ head $ filter ((==x).fst) xs) /= True then (xs, (x,True):ys) 
														else (xs,ys)
generaPeor (Negation y) (xs,ys) = generaPeor y (xs,ys)
generaPeor (Variable x) (xs,ys) = if not $ elem x (map fst xs) then ((x,False):xs,ys)
									else if (snd $ head $ filter ((==x).fst) xs ) /= False then (xs, (x,False):ys) 
											else (xs,ys)
generaPeor (Conjunction x y) t = generaPeor x (generaPeor y t)
generaPeor (Disjunction x y) t = generaPeor x (generaPeor y t)
generaPeor (Implication (Variable x) y) t = generaPeor (Negation (Variable x)) (generaPeor y t)
generaPeor (Implication x y) t = generaPeor x (generaPeor y t)
-}

aux :: [String] -> [Environment]

aux = foldr (\x y -> (map ((x,True):) y) ++ (map ((x,False):) y)) [[]] 


--isTautology :: Proposition -> Bool

--isTautology p =  genera (vars p)
isTautology p =  foldr f True (map (\x -> evalP x p) (aux(vars p)))

	where 
		f (Just a) b = a && b
		f _ _ 		= error "No está definido"


x = Conjunction (Variable "a") (Variable "b")
y = Conjunction (Variable "c") (Variable "d")
z = Disjunction x y

w = Conjunction (Constant True) (Constant True)
w1 = Disjunction (Variable "b") (Constant True)
w2 = Conjunction (Constant True) (Variable "b")
w3 = Implication z w1
w4 = Conjunction (Constant False) (Variable "b")
w5 = Disjunction (Constant True) (Conjunction ( Variable "a") (Variable "b"))

--3.76 b
deb = Implication (Conjunction (Variable "p") (Variable "q")) (Variable "p")

--Una contradicción random
contrad = Conjunction (Implication (Variable "p") (Variable "q")) (Conjunction (Variable "p") (Negation (Variable "q")))

--Contingencia
conting= Disjunction (Variable "p") (Implication (Variable "q") (Variable "r"))

-- Implicaciones (tautologia)

impl = Implication (Conjunction (Implication (Variable "p") (Variable "q")) (Implication (Variable "q") (Variable "r"))) (Implication (Variable "p") (Variable "r"))

-- Random (tautologia)

rand1= Implication (Conjunction (Disjunction (Variable "p") (Variable "q")) 
					(Negation (Variable "q"))) (Variable "p")

-- (((p -> q) & (¬ p -> r)) & (¬ q -> ¬ r)) -> q

rand2 = Implication 
			(Conjunction 
				(Conjunction (Implication (Variable "p") (Variable "q")) (Implication (Negation (Variable "p")) (Variable "r"))) 
				(Implication (Negation (Variable "q")) (Negation (Variable "r")))) 
			(Variable "q")





