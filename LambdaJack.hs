{-
	LambdaJack.hs
	Módulo con los tipos de datos y funciones necesarias para implementar las 
	reglas de Lambda-Jack
	Realizado por:	Richard Lares 		11-10508
					Patricia Reinoso 	11-10851
-}

module LambdaJack (Player (LambdaJack, You), 
	   value, 
	   busted, 
	   winner, 
	   fullDeck, 
	   draw, 
	   playLambda, 
	   shuffle)
		where

import Cards
import qualified System.Random as R


-- Tipo de datos para los dos jugadores de Lambda-Jack
data Player = LambdaJack | You deriving (Show)


-- La función 'value' recibe una mano y retorna el valor numérico de ella
value :: Hand -> Int
value (H xs) = if fst res > 21 
			   then fst res - (snd res) * 10 
			   else fst res
				-- res es una tupla cuyo fst contiene el valor numérico de la 
				-- mano, y snd contiene la cantidad de Aces de la mano
 				where res = foldr (\x (y1,y2) -> 	
					let v = cardValue x 
						-- La función cardValue determina el valor numérico de 
						-- una carta según las reglas de Lambda-Jack
						where
							cardValue (Card (Numeric n) _)	= n
							cardValue (Card Ace _)			= 11
							cardValue (Card _ _)			= 10
					-- Si el valor numérico es mayor a 21, 
 					-- los Aces valen 1 en lugar de 11
					in if v==11 
					   then (y1+v,y2+1) 
					   else (y1+v,y2)) (0,0) xs 


-- La función 'busted' recibe una mano e indica si "explotó" por exceder 21 																			
busted :: Hand -> Bool
busted h = value h > 21


{-
 La función 'winner' compara la mano del jugador con la de Lambda para 
 determinar el ganador. 
 h1 :: Hand = Mano de lambda
 h2 :: Hand = Mano del jugador
 Retorna: Jugador ganador.
-}
winner :: Hand -> Hand -> Player
winner h1 h2 = if busted h1 && busted h2   then LambdaJack 		
			   else if busted h2           then LambdaJack 				
			   else if busted h1 		   then You 					
			   else if value h1 < value h2 then You 	
			   else LambdaJack 					
												

-- La función 'fullDeck' devuelve un mazo completo de la baraja
fullDeck :: Hand
fullDeck = H [Card x y | x<-map (Numeric)[2..10] ++ [Jack , Queen , King , Ace],
				         y<-[Clubs , Diamonds , Spades , Hearts] ]


{-
 La función 'draw' retira la primera carta del mazo de cartas y la añade a la
 mano del jugador, si es posible
 Arg1 :: Hand = Mazo a repartir
 Arg2 :: Hand = Mazo del jugador
 Retorna = Tupla cuyo primer argumento es el mazo restante y el segundo la mano
           del jugador
-} 
draw :: Hand -> Hand -> Maybe (Hand,Hand)
draw d@(H []) h 	   = Just (d,h)						
draw (H (x:xs)) (H ys) = Just (H xs, (H (x:ys)))	
													

--  La función 'playLambda' recibe el mazo y retorna la mano de Lambda
playLambda :: Hand -> Hand
playLambda d = auxL d (H [])
	where	
		-- Si el mazo está vacío, se devuelve la mano sin modificar; 
		-- si no lo está, se añaden cartas a la mano de Lambda hasta que el 
		-- valor de la misma sea mayor o igual a 16
		auxL (H []) h = h 	
		auxL (H (x:xs)) h@(H ys) = if value (H (x:ys)) >=16 
								   then (H (x:ys)) 
								   else auxL (H xs) (H (x:ys))
								   
		
{- 
 La función 'shuffle' recibe un valor StdGen (asiste en la generación de 
 números al azar), un mazo sin barajar y retorna el mazo barajado
-}
shuffle :: R.StdGen -> Hand -> Hand
-- El acumulador del fold es una tripleta con el mazo acumulador, el generador, y el mazo original, respectivamente
shuffle g (H a) = first $ foldl checkCard ((H []),g,a) a 
	where 
			-- La función checkCard verifica si una carta escogida al azar del mazo original está en el mazo acumulador
			checkCard (n@(H ns),g,a) x = 	let 
												ng = R.randomR (0,51) g -- Tupla con un número al azar y un nuevo generador
												ne = a !! fst ng 		-- Elemento escogido al azar
											in 	if elem ne ns 
													-- Si el elemento ya está, se busca otro
													then checkCard (n,snd ng,a) x
													-- Si el elemento no está, se agrega al mazo acumulador
													else ((H (ne:ns)),snd ng,a) 
			-- La función first devuelve el primer elemento de una tripleta
			first (a,b,c) 		= a

shuffle2 :: R.StdGen -> Hand -> (Hand,Hand,R.StdGen)
shuffle2 g (H a) = foldl (\x y -> (checkCard x y)) (H [],H[],g) a
	where
		checkCard (h1@(H xs),h2@(H ys), g) y = let 
												ng = R.randomR (0,1) g

												in if ((fst ng) == 0)
												   then ((H (y : xs)), h2, snd ng)
												   else (h1,(H (y : ys)),snd ng)

		-- first (a,b,c) 		= a
{- PRUEBAS -} 

differents (H xs) = and $ map (/=head xs) (tail xs)

me = H [ Card Ace Hearts, Card King Hearts]
pc = H [ Card Ace Hearts, Card King Hearts, Card Ace Diamonds, Card King Diamonds]