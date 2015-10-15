{-
	hof.hs
	Archivo que contiene diferentes implantaciones de la función filter de Haskell utilizando diferentes estrategias
	Hecho por:	Richard Lares 		11-10508
				Patricia Reinoso 	11-10851
-}

-- Implantación de filter utilizando listas por comprensión
filterC :: (a -> Bool) -> [a] -> [a]
filterC p xs = [x | x<-xs, p x]

-- Implantación de filter utilizando map (y otras funciones auxiliares)
filterM :: (a -> Bool) -> [a] -> [a]
filterM p xs = concat $ zipWith (\x y-> if y then [x] else []) xs (map p xs)

-- Implantación de filter utilizando foldr
filterF :: (a -> Bool) -> [a] -> [a]
filterF p = foldr (\x y -> if p x then x:y else y) [] 