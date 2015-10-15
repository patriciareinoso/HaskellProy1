{-
	lambdajack.hs
	Módulo que implementa un programa ejecutable de Lambda-Jack
	Hecho por:	Richard Lares 		11-10508
				Patricia Reinoso 	11-10851
-}
module Main where

import LambdaJack
import qualified System.Random as R
import Cards
import System.IO

-- Tipo de datos para representar el estado de un juego
data GameState = GS {
						games     :: Int,		-- Cantidad de partidas jugadas
						lambdaWins :: Int,		-- Cantidad de partidas ganadas por Lambda
						name      :: String,	-- Nombre del jugador
						generator :: R.StdGen  	-- Generador de números al azar
					} deriving (Show)

-- La función welcome da un mensaje de bienvenida al juego
welcome :: IO String
welcome = return "Bienvenido a LambdaJack"

-- La función currentState muestra cuántas partidas se han jugado, cuántas ha ganado Lambda y cuántas ha ganado el jugador
currentState :: GameState -> IO ()
currentState g = putStrLn ( "\nDespués de " ++ (show.games)g ++ 
							" partidas Lambda ha ganado "  ++
							(show.lambdaWins)g ++ " y " ++ (name)g ++ 
							" ha ganado " ++ show ((games)g - (lambdaWins)g))

-- La función continuePlaying pregunta al jugador si desea seguir jugando o no, retornando un booleano con la respuesta
-- para continuar con el juego o finalizar su ejecuición
continuePlaying :: IO Bool
continuePlaying = do
	putStrLn "\n¿Desea seguir jugando? [s/n]"
	hSetBuffering stdin NoBuffering		-- Coloca el handle especificado (entrada estándar) en el modo NoBuffering
	x <- getChar         				-- El jugador toma la decisión de si desea seguir jugando o no
	options x
	-- La función options devuelve True si el jugador desea finalizar el juego ('s' 'S') o False en caso contrario ('n' 'N')
	-- Si se introduce cualquier otra opción se repite la pregunta
	where options x 
		|x == 's' || x == 'S' = return True
		|x == 'n' || x == 'N' = return False
		|otherwise = continuePlaying

-- La función anotherCard representa el turno del jugador
anotherCard :: String -> Hand -> Hand -> IO (Hand, Hand)
anotherCard s h1 h2 = do
	if (busted h2)
	then return (h1,h2) 			-- Si el jugador "explotó", no puede pedir más cartas
	else do
	playerMsg s h2 					-- Se muestra un mensaje con la mano del jugador
	putStrLn $ "¿Carta o Listo? [c/l]"
	hSetBuffering stdin NoBuffering -- Coloca el handle especificado (entrada estándar) en el modo NoBuffering
	x <- getChar 					-- El jugador toma la decisión de si desea otra carta o está "listo" 
	options x
	-- La función options llama recursivamente a anotherCard tomando una nueva carta del mazo (getCard) si el jugador desea 
	-- tomar una nueva carta ('c' 'C') o retorna el mazo y la mano actual en caso contrario ('l' 'L')
	-- Si se introduce cualquier otra opción se repite la pregunta
	where options x 
		|x == 'c' || x == 'C' = anotherCard s (fst (getCard h1 h2)) (snd (getCard h1 h2))
		|x == 'l' || x == 'L' = return (h1,h2)
		|otherwise = anotherCard s h1 h2

-- La función getCard inicializa la mano del jugador, y luego permite
-- tomar una carta del mazo (utilizando la función draw del módulo LambdaJack) 
getCard :: Hand -> Hand -> (Hand, Hand)
getCard h1 h2 = if size h2 < 1
				then getCard (fst (f  (draw h1 h2))) (snd (f(draw h1 h2)))
				else f (draw h1 h2)
				where 
					f (Just (x,y)) = (x,y)

-- La función playerMsg muestra un mensaje indicando la mano del jugador y su valor
playerMsg :: String -> Hand -> IO ()
playerMsg s h = do
	putStr $ "\n" ++ s ++ ", tu mano es " ++ show h ++ ", suma " ++ show (value h) ++ ". "

-- La función lambdaMsg muestra un mensaje indicando la mano de Lambda y su valor
lambdaMsg :: Hand -> IO ()
lambdaMsg h = do 
	putStrLn $ "\nMi mano es " ++ show h ++ ", suma " ++ show (value h) ++ ". "

-- La función updateStatus actualiza el gamestate del juego dependiendo de quién ganó la última partida jugada
updateState :: Hand -> Hand -> GameState -> IO GameState
updateState h1 h2 g = do
	if (value h1) == (value h2)
	then isTie g 							-- Si hubo empate o ambos "explotaron", se llama a isTie
	else f (winner h1 h2)
		where 
			f LambdaJack = winnerLambda g 	-- Si ganó Lambda, se llama a winnerLambda (actauliza el gamestate de la misma
											-- forma, pero el mensaje que se muestra es distinto)
			f You 		 = winnerYou g 		-- Si ganó el jugador, se llama a winnerYou

-- La función isTie actualiza el gamestate y muestra un mensaje de empate (igual gana Lambda)
isTie :: GameState -> IO GameState
isTie g = do
	putStrLn "\nEmpatamos, así que yo gano"
	return (GS ((games)g+1) ((lambdaWins)g+1) ((name)g) ((generator)g))

-- La función winnerLambda actualiza el gamestate y muestra un mensaje de Lambda como ganador
winnerLambda :: GameState -> IO GameState
winnerLambda g  = do
	putStrLn "\nYo gano"
	return (GS ((games)g+1) ((lambdaWins)g+1) ((name)g) ((generator)g))

-- La función winnerYou actualiza el gamestate y muestra un mensaje del jugador como ganador
winnerYou :: GameState -> IO GameState
winnerYou g  = do
	putStrLn "\nTu ganas"
	return (GS ((games)g+1) ((lambdaWins)g) ((name)g) ((generator)g))

-- La función gameloop representa el ciclo de una partida que finaliza cuando el jugador lo indique 
-- (sea durante la ejecución normal del programa o mediante una interrupción por teclado)
gameloop :: GameState -> IO ()
gameloop g = do

	-- Se prepara un mazo nuevo, luego se baraja, y finalmente se genera una mano inicial con dos cartas para el jugador
	gen<-R.newStdGen
	--cambiar gen por ((generator)g)
	let initial = getCard (shuffle gen fullDeck) empty
	
	-- Se pregunta al jugador si desea otra carta o "se queda"
	changeturn <- anotherCard ((name)g) (fst initial) (snd initial)
	
	-- Se muestra por última vez la mano del jugador
	playerMsg ((name)g) (snd changeturn)

	if busted (snd changeturn)
	then putStrLn "Perdiste."	-- Si el jugador "explotó", se le indica que perdió
	else putStrLn "Mi turno."
	
	-- Turno de Lambda (playLambda) con el mazo restante (fst changeturn)
	let lambdahand = playLambda (fst changeturn) 

	-- Muestra la mano final de Lambda
	lambdaMsg lambdahand

	-- Actualiza gamestate y muestra mensaje correspondiente
	newstate <- updateState lambdahand (snd changeturn) g

	-- Se muestra el estado actual del juego
	currentState newstate

	-- Se pregunta al jugador si desea continuar con el juego
	x <- continuePlaying
	if x 
	then gameloop newstate 					-- En caso afirmativo, se repiten las instrucciones anteriores
	else putStrLn "\n\nFin del juego\n"		-- En caso negativo, se muiestra mensaje de Fin del juego

-- main es una secuencia de instrucciones que representan Lambda-Jack
main =	welcome >>= (\c -> putStrLn c) >> putStrLn "\n¿Cómo te llamas?" >> getLine 
				>>= (\name->gameloop (GS 0 0 name (R.mkStdGen 42)))

{- PRUEBAS -}

p = GS 0 0 "Patty" (R.mkStdGen 42)
x1 = Clubs
x2 = Diamonds
x3 = Spades
x4 = Hearts

y1 = Jack
y2 = Numeric 10
y3 = Numeric 5

z1 = Card y1 x1
z2 = Card y2 x2
z3 = Card y3 x3

h1 = H [z1]
h2 = H [z2]
h3 = H [z3]