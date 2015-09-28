filterC :: (a -> Bool) -> [a] -> [a]
filterC p xs = [x | x<-xs, p x]

filterM :: (a -> Bool) -> [a] -> [a]
filterM p xs = concat $ zipWith (\x y-> if y then [x] else []) xs (map p xs)

filterF :: (a -> Bool) -> [a] -> [a]
filterF p = foldr (\x y -> if p x then x:y else y) [] 
