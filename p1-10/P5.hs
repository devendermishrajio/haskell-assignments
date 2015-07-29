module P5
where
myReverse [] = []
myReverse (x:xs) = myReverse(xs)++[x]

