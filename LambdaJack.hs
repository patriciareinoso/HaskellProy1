{-
	LambdaJack.hs
	Módulo con los tipos de datos y funciones necesarias para implementar las reglas de Lambda-Jack
	Hecho por:	Richard Lares 		11-10508
				Patricia Reinoso 	11-10851
-}

module LambdaJack (Player (LambdaJack, You), value, busted, winner, fullDeck, draw, playLambda, shuffle)
	where

import Cards
import qualified System.Random as R

-- Tipo de datos para los dos jugadores de Lambda-Jack
data Player = LambdaJack | You deriving (Show)

-- La función value determina el valor numérico de una mano
value :: Hand -> Int
value (H xs) = if fst res > 21 then fst res - (snd res) * 10 else fst res
				-- res es una tupla cuyo fst contiene el valor numérico de la mano, 
				-- y snd contiene la cantidad de Aces de la mano
 				where res = foldr (\x (y1,y2) -> 	
					let v = cardValue x 
						-- La función cardValue determina el valor numérico de una carta según las reglas de Lambda-Jack
						where
							cardValue (Card (Numeric n) _)	= n
							cardValue (Card Ace _)			= 11
							cardValue (Card _ _)			= 10
					in if v==11 then (y1+v,y2+1) else (y1+v,y2)) (0,0) xs 	-- Si el valor numérico es mayor a 21, 
 																			-- los Aces valen 1 en lugar de 11
-- La función busted indica si una mano "explotó" por exceder 21 																			
busted :: Hand -> Bool
busted h = value h > 21

-- La función winner compara la mano del jugador (h2) con la de Lambda (h1) para determinar el ganador 
winner :: Hand -> Hand -> Player
winner h1 h2 = if busted h1 && busted h2 then LambdaJack 		-- Si los jugadores "explotan", gana Lambda
				else if busted h2 then LambdaJack 				-- Si el jugador explota, gana Lambda
					else if busted h1 then You 					-- Si sólo explota Lambda, gana el jugador
						else if value h1 < value h2 then You 	-- Si ninguno explota, gana el jugador si el valor de su
							else LambdaJack 					-- mano es mayor estricto al valor de la mano de Lambda,
																-- y gana Lambda eoc

-- La función fullDeck devuelve un mazo completo de la baraja en cualquier orden
fullDeck :: Hand
fullDeck = H [ Card x y | x<-map (Numeric) [2..10] ++ [Jack , Queen , King , Ace], y<-[Clubs , Diamonds , Spades , Hearts]  ]

-- La función draw retira la primera carta del mazo de cartas y la añade a la mano del jugador, si es posible
draw :: Hand -> Hand -> Maybe (Hand,Hand)
draw d@(H []) h = Just (d,h)						-- Si el mazo está vacío, se devuelve una tupla con el mismo y la 
													-- mano del jugador sin modificar
draw (H (x:xs)) (H ys) = Just (H xs, (H (x:ys)))	-- Si el mazo no está vacío, se devuelve una tupla con el mazo
													-- sin su primera carta ya que fue añadida a la mano del jugador

-- La función playLambda produce la mano de Lambda siguiendo las reglas con las que debe jugar a partir del mazo
-- que quedó después de la jugada del jugador, valga la redundancia
playLambda :: Hand -> Hand
playLambda d = auxL d (H [])
	where	
		-- Si el mazo está vacío, se devuelve la mano sin modificar; si no lo está, se añaden cartas a la mano de
		-- Lambda hasta que el valor de la misma sea mayor o igual a 16
		auxL (H []) h = h 	
		auxL (H (x:xs)) h@(H ys) = if value (H (x:ys)) >=16 then (H (x:ys)) else auxL (H xs) (H (x:ys))
		
-- La función shuffle recibe un mazo sin barajar y lo baraja		
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

{- PRUEBAS -} 

differents (H xs) = and $ map (/=head xs) (tail xs)

me = H [ Card Ace Hearts, Card King Hearts]
pc = H [ Card Ace Hearts, Card King Hearts, Card Ace Diamonds, Card King Diamonds]