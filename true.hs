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
a = [("x",True),("y",False)] :: Environment


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
		f (Constant b) _ 		= []
		f (Variable s) xs 		= if elem s xs then xs else s : xs
		f (Negation p) xs 		= f p xs
		f (Conjunction p q) xs  = f p (f q xs)
		f (Disjunction p q) xs  = f p (f q xs)
		f (Implication p q) xs  = f p (f q xs)

--isTautology :: Proposition -> Bool