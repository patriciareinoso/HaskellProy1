import qualified System.Random as R

data GameState = GS {
						games     :: Int,
						lamdaWins :: Int,
						name      :: String--,
						--generator :: R.StdGen
					} deriving (Show)

-- welcome :: IO String  putStrLn es :: IO() :S :S :S
-- si pongo return es IO String pero lo imprime con las comillas
welcome :: IO String

welcome = return "Bienvenido a LamdaJack"

currentState :: GameState -> IO ()

currentState g = putStrLn $ "Después de " ++ (show.games)g ++ " partidas Lambda ha ganado " ++ (show.lamdaWins)g ++ " y " ++ (name)g ++ " ha ganado " ++ show ((games)g - (lamdaWins)g)

continuePlaying :: IO Bool

continuePlaying = do
	putStrLn "¿Desea seguir jugando? [s/n]\n"
	r <- getChar 
	if r == 's' 
	then return True 
	else 
		if r == 'n' 
		then return False
		else continuePlaying
	

-- gameloop :: GameState -> IO ()

g = GS 7 5 "Patty"