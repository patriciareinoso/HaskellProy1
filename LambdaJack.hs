import Cards as C

data Player = LambdaJack | You deriving (Show)

value :: Hand -> Int
value (H xs) =  let res = foldr (\x (y1,y2) -> let v = C.cardValue x in if v==11 then (y1+v,y2+1) else (y1+v,y2)) (0,0) xs
					in if fst res > 21 then fst res - (snd res) * 10 else fst res

busted :: Hand -> Bool
busted h = value h > 21

winner :: Hand -> Hand -> Player
winner h1 h2 = if busted h1 && busted h2 then LambdaJack
				else if busted h2 then You
					else if busted h1 then LambdaJack
						else if value h1 > value h2 then You
							else LambdaJack 

fullDeck :: Hand
fullDeck = H [ Card x y | x<-map (Numeric) [1..10] ++ [Jack , Queen , King , Ace], y<-[Clubs , Diamonds , Spades , Hearts]  ]

me = H [ Card Ace Hearts, Card King Hearts]
pc = H [ Card Ace Hearts, Card King Hearts, Card Ace Diamonds, Card King Diamonds]

-- draw :: Hand -> Hand -> Maybe (Hand,Hand)

-- playLambda :: Hand -> Hand

-- shuffle :: StdGen -> Hand -> Hand