{-
	true.hs
	Archivo con la implementación de un verificador de tautologías (expresiones de Lógica Proposicional de Primer Orden)
	Hecho por:	Richard Lares 		11-10508
				Patricia Reinoso 	11-10851
-}

-- Tipo de datos recursivo monomórfico para representar expresiones de lógica proposicional
data Proposition = Constant Bool						-- Constructor para las constantes booleanas (:: Bool)
				 | Variable String						-- Constructor para las variables (::String)
				 | Negation Proposition 				-- Constructor para la negación de una expresión
				 | Conjunction Proposition Proposition 	-- Constructor para la conjunción de dos expresiones
				 | Disjunction Proposition Proposition 	-- Construtor para la disyunción de dos expresiones
				 | Implication Proposition Proposition	-- Constructor para la impliciación de dos expresiones
				 deriving (Show)

-- Tipo sinónimo para ambientes de evaluación de una proposición lógica
type Environment = [(String,Bool)]

-- La función find busca una variable en un ambiente de evaluación y devuelve su valor (si la misma existe)
find :: Environment -> String -> Maybe Bool
find [] _ = Nothing				-- Si el ambiente de evaluación no tiene variables, devuelve Nothing
find ((x,y):l) k
	| x == k 		= Just y 	-- Si la variable fue encontrada, se devuelve su valor (con Just)
	| otherwise 	= find l k	-- Si no ha sido encontrada aún, se busca en el resto del ambiente

-- La función addOrReplace añade una variable a un ambiente, o modifica el valor de ésta si ya existía en dicho ambiente
addOrReplace :: Environment -> String -> Bool -> Environment
addOrReplace l x b = 	if find l x == Nothing	
							then (x,b):l 			-- Si la variable no existía se añade al principio del ambiente
							else addAux l [] x b 	-- Si ya existía se añade utilizando addAux (auxiliar)
	where 	
		addAux [] l x b = reverse l 				-- Si ya se recorrió todo el ambiente, se retorna el mismo con la
													-- variable modificada
		addAux (t@(l,m):n) q x b
			| l == x 	= addAux n ((x,b):q) x b 	-- Si la variable fue encontrada, se modifica su valor
			| otherwise	= addAux n (t:q) x b 		-- Si no ha sido encontrada aún, se busca en el resto del ambiente
		
-- La función remove elimina una variable de un ambiente		
remove :: Environment -> String -> Environment
remove e k = remAux e k []
	where 	
		remAux [] _ l 				= reverse l 			-- Se devuelve el ambiente recorrido (si la variable fue
															-- eliminada o no porque no fue encontrada)
		remAux ((x,y):m) k l 
			| x==k 					= remAux m k l 			-- Si la variable fue encontrada, no se agrega al nuevo
															-- ambiente
			| otherwise				= remAux m k ((x,y):l)	-- Si no fue encontrada, se busca en el resto

a = [("x",True),("y",False)] :: Environment

-- La función evalP recorre una proposición apoyándose en un ambiente de evaluación y calcula su valor de verdad
evalP :: Environment -> Proposition -> Maybe Bool

-- Si la expresión es una constante, se devuelve su valor
evalP _ (Constant b) 	  = Just b 		

-- Si la expresión no es una constante, se busca su valor en el ambiente de evaluación mediante la función find								
evalP e (Variable s)      = find e s 
evalP e (Negation p)      = f (evalP e p)	
							where 
								-- Si el valor es encontrado, se niega
								f (Just p) = Just (not p)
								f _ 	   = Nothing
evalP e (Conjunction p q) = f (evalP e p) (evalP e q)
							where
								-- Si ambos valores son encontrados, se aplica and entre ellos
								f (Just p) (Just q) = Just (p && q)
								f _ _ 				= Nothing
evalP e (Disjunction p q) = f (evalP e p) (evalP e q)
							where
								-- Si ambos valores son encontrados, se aplica or entre ellos
								f (Just p) (Just q) = Just (p || q)
								f _ _ 				= Nothing

evalP e (Implication p q) = f (evalP e p) (evalP e q)
							where 
								-- Si ambos valores son encontrados, se implican (sólo es False cuando
								-- el antecedente es True y el consecuente es False)
								f Nothing _ 		       = Nothing
								f _ Nothing 		       = Nothing
								f (Just True) (Just False) = Just False
								f _ _				       = Just True

-- La función vars extrae los nombres de variables de una proposición sin repetirlos								
vars :: Proposition -> [String]
vars p = f p []
	where
		f (Constant b) xs 		= xs								-- Si la expresión es una constante, se devuelve la
																	-- lista original
		f (Variable s) xs 		= if elem s xs then xs else s : xs	-- Si la expresión es una variable, se verifica
																	-- si la variable ya estaba en la lista de variables
																	-- para agregarla a la misma o no
		f (Negation p) xs 		= f p xs							-- Si la expresión es unaria, se verifica la expresión
		f (Conjunction p q) xs  = f p (f q xs)						-- Si la expresión es binaria, se verifican ambas
		f (Disjunction p q) xs  = f p (f q xs)
		f (Implication p q) xs  = f p (f q xs)

-- La función isTautology genera todos los ambientes de evaluación posibles para una proposición lógica dada, y determina
-- si ésta es una tautología
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