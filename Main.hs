data GameState = GS {
						games     :: Int,
						lamdaWins :: Int,
						name      :: String,
						generator :: StdGen
					}

-- welcome :: IO String

-- currentState :: GameState -> IO ()

-- continuePlaying :: IO Bool

-- gameloop :: GameState -> IO ()