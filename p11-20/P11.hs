module P11
where
import P9

--Problem 11
data Items = Multiple Int Char|Single Char
		deriving(Show,Eq)

encodeModified l = map (\x-> if length x > 1 then Multiple (length x) (head x) else Single (head x)) (pack l)


--Problem 12
decodeModified = foldl (\acc elem -> case elem of
	Multiple x c -> acc ++ replicate x c
	Single c -> acc ++ [c]) []

--Problem 13
-- @todo

--Problem 14
dupli = foldl (\acc elem -> acc ++ [elem, elem]) []

--Problem 15
repli l n = foldl (\acc elem -> acc ++ replicate n elem) [] l

--Problem 16
dropEvery l n = dropEveryT l n 0
	where
	 dropEveryT ([]) n i = []
	 dropEveryT (x:xs) n i = if i==(n-1)
	 then dropEveryT xs n 0
	 else x:dropEveryT xs n (i+1)

--Problem 17
split l n = ( take n l, (reverse (take ((length l) - n) (reverse l))))

--Problem 18
slice l m n = ( take (n-m+1) (drop (m-1) l))

--Problem 19
rotate l n = if n < 0
	 then rotate l (length l + n)
	 else reverse (reverse(slice l 1 n) ++ reverse(slice l (n+1) (length l)))

--Problem 20
removeAt k l = (l!!(k-1), (slice l 1 (k-1)) ++ (slice l (k+1) (length l)))
 
