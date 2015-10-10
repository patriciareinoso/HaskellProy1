module Cards ( Hand(H), Suit (Clubs,Diamonds,Spades,Hearts), Value (Numeric,Jack,Queen,King,Ace), Card(Card),
				empty, size, cardValue ) where

data Suit = Clubs | Diamonds | Spades | Hearts deriving(Read,Eq)

instance Show Suit where
	-- show Clubs    = "♣" 
	show Clubs    = "♧" 
	show Diamonds = "♦" 
	--show Spades   = "♠" 
	show Spades   = "♤" 
	show Hearts   = "♥"

data Value = Numeric Int | Jack | Queen | King | Ace deriving (Read,Eq)

instance Show Value where
	show (Numeric n) = show n
	show Jack 	= "J"
	show Queen 	= "Q"
	show King 	= "K"
	show Ace 	= "A"

data Card = Card {
					value :: Value,
					suit :: Suit
				 }
				 deriving (Read)

instance Show Card where
	--show (Card v s) = show s ++ show v
	show c = (show.suit)c ++ (show.value)c

instance Eq Card where
	(==) (Card a b) (Card c d) 	= (a==c) && (b==d)
	(/=) c v 					= not (c==v)

newtype Hand = H [Card]
				deriving (Read)

instance Show Hand where
	show (H []) 	= ""
	show (H (x:xs)) = show x ++ " " ++ show (H xs)

empty :: Hand

empty = H []

size :: Hand -> Int

size (H h) = length h

cardValue :: Card -> Int

cardValue (Card (Numeric n) _)	= n
cardValue (Card Ace _)			= 11
cardValue (Card _ _)			= 10

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

w = H [z1,z2,z3]
y = Numeric 10

z = Card y x4

w2 = H [z,z,z]
r = H [ Card Ace Diamonds, Card (Numeric 9) Spades, Card Jack Hearts]
