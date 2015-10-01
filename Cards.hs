data Suit = Clubs | Diamonds | Spades | Hearts deriving(Read)

instance Show Suit where
	show Clubs    = "C" 
	show Diamonds = "D" 
	show Spades   = "S" 
	show Hearts   = "H"

data Value = Numeric Int | Jack | Queen | King | Ace deriving (Read)

instance Show Value where
	-- show (Numeric Int) = (Numeric Int)
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
				deriving (Show, Read)

--instance Show Hand where
--	show (H (x:xs)) = show xs ++ show x 

empty :: Hand

empty = H []

size :: Hand -> Int

size (H h) = length h

x = Diamonds
y = Jack

z = Card y x

w = H [z,z,z]