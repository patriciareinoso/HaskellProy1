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
	playerMsg s h
	putStrLn $ ". ¿Carta o Listo? [c/l]"
	r <- getChar
	options r

	where options x 
		|x == 'c' || x == 'C' = return True
		|x == 'l' || x == 'L' = return False
		|otherwise = anotherCard s h

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


gameloop :: GameState -> IO ()

gameloop g = do
 
	let mazo = shuffle ((generator)g) fullDeck
	print mazo
	anotherCard "Patty" empty
	lambdaMsg empty

-- y genera una mano inicial con dos cartas para el jugador

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