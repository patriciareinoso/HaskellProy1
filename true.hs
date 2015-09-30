-- type Variable = String

data Proposition = Value Bool
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

evalP _ (Value b) 	 = Just b
evalP e (Variable s) = find e s 
-- evalP e (Negation p) = Just (evalP e p)
-- evalP e (Conjunction p q) = (evalP e p) && (evalP e q)
-- evalP e (Disjunction p q) = (evalP e p) || (evalP e q)
-- evalP e (Implication p q) = f (evalP e p) (evalP e q)

-- where 
-- 	f True False = Just False
-- 	f _ _ 		 = Just True

--vars :: Proposition -> [String]

--isTautology :: Proposition -> Bool