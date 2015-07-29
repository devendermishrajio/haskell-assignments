module P6
where
isPalindrome ([]) = True
isPalindrome ([_]) = True
isPalindrome (x:xs) = x==last(xs) && isPalindrome (init xs)

