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
	r <- getChar 
	options r
	
	where options x 
		|x == 's' || x == 'S' = return True
		|x == 'n' || x == 'N' = return False
		|otherwise = continuePlaying


anotherCard :: String -> Hand -> IO Bool

anotherCard s h = do
	putStrLn $ "\n" ++ s ++ ", tu mano es " ++ show h ++ " Suma " ++ show (value h) ++ ". ¿Carta o Listo? [c/l]"
	r <- getChar
	options r

	where options x 
		|x == 'c' || x == 'C' = return True
		|x == 'l' || x == 'L' = return False
		|otherwise = anotherCard s h

gameloop :: GameState -> IO ()

gameloop g = do
 
	let mazo = shuffle ((generator)g) fullDeck
	print mazo

-- y genera una mano inicial con dos cartas para el jugador

p = GS 0 0 "Patty" (R.mkStdGen 42)