module P2
where
--myButLast = last.init
myButLast [x,_] = x
myButLast (x:xs) = myButLast xs
