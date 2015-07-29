module P3
where
elementAt (x:_) 1 = x
elementAt (_:xs) k = elementAt xs (k-1)

