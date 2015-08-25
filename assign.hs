cube x = x*x*x
tc_lim = 100
taxicab = [(cube a + cube b)
		| a <- [1..tc_lim],
		  b <- [(a+1)..tc_lim],
		  c <- [(a+1)..tc_lim],
		  d <- [(c+1)..tc_lim],
		  (cube a + cube b) == (cube c + cube d)]

ram n = take n taxicab
ramanujan n = (ram n)!!(n-1)

is_matrix (x:[]) = (length x) /= 0
is_matrix [] = False
is_matrix (x:y:xs) = (length x) == (length y) && (is_matrix (y:xs))

is_square_matrix n = (is_matrix n) && ((length (n!!0)) == (length n))


addable m n = (is_matrix n) && (is_matrix m) && ((length n) == (length m)) && ((length (n!!0)) == (length (m!!0)))

add_matrix m n = if addable m n then add_matrix_sub m n else [[]]
	where
		add_matrix_sub (x:[]) (y:[]) = x+y
		add_matrix_sub (x:xs) (y:ys) = add_list x y ++ add_matrix_sub xs ys
			where
				add_list (x:[]) (y:[]) = [x+y]
				add_list (x:xs) (y:ys) = (x+y):add_list xs ys
