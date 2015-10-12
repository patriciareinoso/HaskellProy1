import LambdaJack as LJ
import qualified System.Random as R
import Cards as C

data GameState = GS {
						games     :: Int,
						lambdaWins :: Int,
						name      :: String,
						generator :: R.StdGen
					} deriving (Show)

-- welcome :: IO String  putStrLn es :: IO() :S :S :S
-- si pongo return es IO String pero lo imprime con las comillas
welcome :: IO String

welcome = return "Bienvenido a LambdaJack"

currentState :: GameState -> IO ()

currentState g = putStrLn ( "Después de " ++ (show.games)g ++ 
							" partidas Lambda ha ganado "  ++
							(show.lambdaWins)g ++ " y " ++ (name)g ++ 
							" ha ganado " ++ show ((games)g - (lambdaWins)g))


continuePlaying :: IO Bool

continuePlaying = do
	putStrLn "\n¿Desea seguir jugando? [s/n]"
	x <- getChar 
	options x
	
	where options x 
		|x == 's' || x == 'S' = return True
		|x == 'n' || x == 'N' = return False
		|otherwise = continuePlaying


anotherCard :: String -> Hand -> IO Bool

anotherCard s h = do
	playerMsg s h
	putStrLn $ ". ¿Carta o Listo? [c/l]"
	x <- getChar
	options x

	where options x 
		|x == 'c' || x == 'C' = return True
		|x == 'l' || x == 'L' = return False
		|otherwise = anotherCard s h

--anotherCard2 :: String -> Hand -> IO()

anotherCard2 s h1 h2 = do
	playerMsg s h2
	putStrLn $ ". ¿Carta o Listo? [c/l]"
	x <- getChar
	options x

	where options x 
		|x == 'c' || x == 'C' = anotherCard2 s (fst (reparte h1 h2)) (snd (reparte h1 h2))
		|x == 'l' || x == 'L' = return (h1,h2)
		|otherwise = anotherCard2 s h1 h2

playerMsg :: String -> Hand -> IO ()

playerMsg s h = do
	putStr $ "\n" ++ s ++ ", tu mano es " ++ show h ++ " , suma " ++ show (value h) 

lambdaMsg :: Hand -> IO ()

lambdaMsg h = do 
	putStrLn $ "\nMi mano es " ++ show h ++ ", suma " ++ show (value h)

winnerMsg :: Hand -> Hand -> IO ()

winnerMsg h1 h2 = if (value h1) == (value h2)
				  then putStrLn "\nEmpatamos, así que yo gano."
				  else f (winner h1 h2) 
				  		where 
				  			f LambdaJack = putStrLn "\nYo gano" 
				  			f You 		 = putStrLn "\nTu ganas"




reparte :: Hand -> Hand -> (Hand, Hand)

reparte h1 h2 = if size h2 < 1
				then reparte (fst (f  (draw h1 h2))) (snd (f(draw h1 h2)))
				else f (draw h1 h2)
				where 
					f (Just (x,y)) = (x,y)

gameloop :: GameState -> IO ()

gameloop g = do
	-- prepara un mazo nuevo, lo baraja, 
	let deck = shuffle ((generator)g) fullDeck
	-- genera una mano inicial con dos cartas para el jugador
	let youdeck = deck
	-- debe presentar las cartas al jugador, indicar su puntuación
	-- preguntar si desea otra carta o «se queda»
	anotherCard ((name)g) youdeck

	putStrLn ". Mi turno."
	let lambdadeck = playLambda deck 
	--muestra su mano final y anuncia el resultado, indicando
	lambdaMsg lambdadeck
	-- Dependiendo del resultado del juego, en el lugar de debe escribirse ‘Yo gano’,
	-- ‘Tu ganas’, ‘Empatamos, así que yo gano.’
	winnerMsg lambdadeck youdeck

	-- ACTUALIZAR GAME STATE 
	currentState g

	x <- continuePlaying
	if x 
	then gameloop (GS ((games)g+1) ((lambdaWins)g) ((name)g) ((generator)g))
	else putStrLn "\nFin del juego"

main = welcome >>= (\c -> putStrLn c) >> putStrLn "\n¿Cómo te llamas?" >> getLine >>= (\name->gameloop (GS 0 0 name (R.mkStdGen 42)))


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