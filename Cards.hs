data Suit = Clubs | Diamonds | Spades | Hearts deriving (Show)

data Value = Numeric Int | Jack | Queen | King | Ace deriving (Show)

data Card = Card {
					value :: Value,
					suit :: Suit
				 }
				 deriving (Show)

newtype Hand = H [Card]
				deriving (Show)

empty :: Hand

empty = H []

--size :: Hand -> Int