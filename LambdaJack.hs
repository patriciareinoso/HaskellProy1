import Cards as C
import qualified System.Random as R
import qualified Data.List as L

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
fullDeck = H [ Card x y | x<-map (Numeric) [2..10] ++ [Jack , Queen , King , Ace], y<-[Clubs , Diamonds , Spades , Hearts]  ]

draw :: Hand -> Hand -> Maybe (Hand,Hand)
draw d@(H []) h = Just (d,h)
draw (H (x:xs)) (H ys) = Just (H xs, (H (x:ys)))

playLambda :: Hand -> Hand
playLambda d = auxL d (H [])
	where	auxL (H []) h = h
		auxL (H (x:xs)) h@(H ys) = if value (H [x]) + value h >=16 then (H (x:ys)) else auxL (H xs) (H (x:ys))
		
shuffle :: R.StdGen -> Hand -> Hand
shuffle g (H a) = first $ foldl checkCard ((H []),g,a) a
	where 
			checkCard (n@(H ns),g,a) x = 	let 
										ng = R.randomR (0,51) g 
										ne = a !! fst ng
									in 	if elem ne ns 
											then checkCard (n,snd ng,a) x
											else ((H (ne:ns)),snd ng,a) 
			first (a,b,c) 		= a

cards (H xs) = xs
cNub (H xs) = L.nub xs 

me = H [ Card Ace Hearts, Card King Hearts]
pc = H [ Card Ace Hearts, Card King Hearts, Card Ace Diamonds, Card King Diamonds]