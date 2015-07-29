module P1
where
myLast [] = error "Empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs
