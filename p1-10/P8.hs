module P8
where
compress ([]) = []
compress ([x]) = [x]
compress (x:x1:xs) = if x==x1 then compress (x:xs) else x:compress (x1:xs)

