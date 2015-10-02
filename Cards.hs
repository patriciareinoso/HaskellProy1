data Suit = Clubs | Diamonds | Spades | Hearts deriving(Read)

instance Show Suit where
	-- show Clubs    = "♣" 
	show Clubs    = "♧" 
	show Diamonds = "♦" 
	--show Spades   = "♠" 
	show Spades   = "♤" 
	show Hearts   = "♥"

data Value = Numeric Int | Jack | Queen | King | Ace deriving (Read)

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

newtype Hand = H [Card]
				deriving (Read)

instance Show Hand where
	show (H []) 	= ""
	show (H (x:xs)) = show x ++ " " ++ show (H xs)

empty :: Hand

empty = H []

size :: Hand -> Int

size (H h) = length h

x1 = Clubs
x2 = Diamonds
x3 = Spades
x4 = Hearts
y = Numeric 10

z = Card y x4

w = H [z,z,z]
r = H [ Card Ace Diamonds, Card (Numeric 9) Spades, Card Jack Hearts]