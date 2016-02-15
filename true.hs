{-
	true.hs
	Archivo con la implementación de un verificador de tautologías 
	(expresiones de Lógica Proposicional de Primer Orden)
	Realizado por:	Richard Lares 		11-10508
					Patricia Reinoso 	11-10851
-}

{-
 Tipo de datos recursivo monomórfico para representar expresiones de lógica 
 proposicional. Constructor para las constantes booleanas, variables, 
 negación, conjunción, disyunción e implicación.
-} 
data Proposition = Constant Bool						
				 | Variable String						
				 | Negation Proposition 				
				 | Conjunction Proposition Proposition 	
				 | Disjunction Proposition Proposition 	
				 | Implication Proposition Proposition	
				 deriving (Show)


-- Tipo para ambientes de evaluación de una proposición lógica
type Environment = [(String,Bool)]


{- 
 La función 'find' recibe un ambien de evaluación y el nombre de una variable.
 Busca la variable en un ambiente de evaluación y devuelve su valor 
 (si la misma existe)
-}
find :: Environment -> String -> Maybe Bool
find [] _ = Nothing				
find ((x,y):l) k
	| x == k 		= Just y 	
	| otherwise 	= find l k


find2 :: Environment -> String -> Maybe Bool
find2 e k = foldl findAux Nothing e
			where 
				findAux (Just a) _ 	= Just a
				findAux _ (str,bool) = if str == k
								 	   then Just bool
								 	   else Nothing


{-
 La función 'addOrReplace' recibe un ambiente de evaluación, una variable y 
 su respectivo valor. Añade la variable al ambiente, o modifica su valor si 
 ésta ya existía en dicho ambiente
-}
addOrReplace :: Environment -> String -> Bool -> Environment
addOrReplace l x b = 	if find l x == Nothing	
							then (x,b):l 			
							else addAux l [] x b 	
	where 	
		addAux [] l x b = reverse l 				
													
		addAux (t@(l,m):n) q x b
			| l == x 	= addAux n ((x,b):q) x b 	
			| otherwise	= addAux n (t:q) x b 		
		

addOrReplace2 :: Environment -> String -> Bool -> Environment
addOrReplace2 e k v = if find e k  == Nothing
					  then (k,v):e
					  else foldl addAux [] $ reverse e

					  where
					  	addAux nenv a@(str,bool) = if str == k 
					  							   then (str,v):nenv
					  							   else a:nenv

{-
 La función 'remove' recibe un ambiente de evaluación y el nombre de una 
 variable.  En caso de que la variable exista en dicho ambiente, la elimina. 
 En caso contrario produce el mismo ambiente sin modificar.	
-}
remove :: Environment -> String -> Environment
remove e k = remAux e k []
	where 	
		remAux [] _ l 				= reverse l 			
		remAux ((x,y):m) k l 
			| x==k 					= remAux m k l 			
			| otherwise				= remAux m k ((x,y):l)	


remove2 :: Environment -> String -> Environment
remove2 e k = if find e k == Nothing
			  then e
			  else foldl remAux [] $ reverse e

			  where
			  	remAux nenv e = if fst e == k
			  					then nenv
			  					else e:nenv


{-
 La función 'evalP' recorre una proposición apoyándose en un ambiente de 
 evaluación y calcula su valor de verdad
-} 
evalP :: Environment -> Proposition -> Maybe Bool

evalP _ (Constant b) 	  = Just b 		
evalP e (Variable s)      = find e s 
evalP e (Negation p)      = evalNegAux (evalP e p)	
							where 
								evalNegAux (Just p) = Just (not p)
								evalNegAux _ 	   = Nothing
evalP e (Conjunction p q) = evalConjAux (evalP e p) (evalP e q)
							where
								evalConjAux (Just p) (Just q) = Just (p && q)
								evalConjAux _ _ 	 		  = Nothing
evalP e (Disjunction p q) = evalDisjAux (evalP e p) (evalP e q)
							where
								evalDisjAux (Just p) (Just q) = Just (p || q)
								evalDisjAux _ _ 			  = Nothing

evalP e (Implication p q) = evalImpAux (evalP e p) (evalP e q)
							where 
								evalImpAux Nothing _ 		        = Nothing
								evalImpAux _ Nothing 		        = Nothing
								evalImpAux (Just True) (Just False) = Just False
								evalImpAux _ _				        = Just True

{-
 La función 'vars' extrae los nombres de variables de una proposición y los 
 retorna en una lista. Si una variable aparece más de una vez en una proposición, 
 la función garantiza que no se repitan.								
-}
vars :: Proposition -> [String]
vars p = varsAux p []
	where
		varsAux (Constant b) xs 	  = xs								
		varsAux (Variable s) xs 	  = if elem s xs then xs else s : xs	
		varsAux (Negation p) xs 	  = varsAux p xs							
		varsAux (Conjunction p q) xs  = varsAux p (varsAux q xs)						
		varsAux (Disjunction p q) xs  = varsAux p (varsAux q xs)
		varsAux (Implication p q) xs  = varsAux p (varsAux q xs)

{-
 La función 'isTautology' genera todos los ambientes de evaluación posibles 
 para una proposición lógica dada, y determina si ésta es una tautología
-}
isTautology :: Proposition -> Bool
isTautology p =  foldr f True (map (\x -> evalP x p) (aux(vars p)))
	where 
		-- aux es una función auxiliar que genera la lista con todos los ambientes de evaluación posibles para una lista
		-- de variables dada
		aux = foldr (\x y -> (map ((x,True):) y) ++ (map ((x,False):) y)) [[]] 
		-- f evalua la conjunción entre un Just Bool y Bool (para aplicarlo con los resultados de evalP de cada ambiente)
		f (Just a) b = a && b
		f _ _ 		= error "No está definido"

{- PRUEBAS -}

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