{-
	Cards.hs
	Módulo con los tipos de datos y funciones necesarias para la manipulación de
	cartas utilizadas en Lambda-Jack
	Hecho por:	Richard Lares 		11-10508
				Patricia Reinoso 	11-10851
-}

module Cards ( Hand(H), 
			   Suit (Clubs,Diamonds,Spades,Hearts), 
			   Value (Numeric,Jack,Queen,King,Ace), 
			   Card(Card),
			   empty,
			   size) where

-- Tipo de datos para representar los "palos" de la baraja francesa: 
data Suit = Clubs | Diamonds | Spades | Hearts deriving (Eq)

-- Se define show para cada constructor, cambiando su nombre por su símbolo
instance Show Suit where
	show Clubs    = "♧" 
	show Diamonds = "♦" 
	show Spades   = "♤" 
	show Hearts   = "♥"

-- Tipo de datos para representar los valores de las cartas: 1-10, J, Q, K, y A
data Value = Numeric Int | Jack | Queen | King | Ace deriving (Eq)

-- Se define show para mostrar el número o letra asociado a cada valor
instance Show Value where
	show (Numeric n) = show n
	show Jack 	= "J"
	show Queen 	= "Q"
	show King 	= "K"
	show Ace 	= "A"

-- Tipo de datos para representar la baraja francesa
data Card = Card {
					value :: Value,
					suit  :: Suit
				 }

-- Se define show para mostrar el palo y el valor de una carta 
instance Show Card where
	show c = (show.suit)c ++ (show.value)c


{-
 Se define eq, de manera que dos cartas son iguales cuando tienen mismo palo y 
 mismo valor, y son diferentes eoc
-}
instance Eq Card where
	(==) (Card a b) (Card c d) 	= (a==c) && (b==d)
	(/=) c v 					= not (c==v)

-- Tipo de datos para modelar la mano de cartas de un jugador
newtype Hand = H [Card]

-- Se define show para mostrar las cartas que tiene un jugador en su "mano"
instance Show Hand where
	show (H []) 	= ""
	show (H (x:xs)) = show x ++ " " ++ show (H xs)

-- La función 'empty' produce una mano vacía
empty :: Hand
empty = H []

-- La función 'size' recibe una mano y determina cuántas cartas tiene
size :: Hand -> Int
size (H h) = length h