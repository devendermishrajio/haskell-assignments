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

make_list m = map (\x -> (map (\y -> y:[]) x)) m
trans_mat m = foldl (\acc x -> zipWith (++) acc x) (head ls) (tail ls)
                where
                        ls = make_list m
mat_mult m n = map (\x -> map (\y -> foldl (+) 0 (zipWith (*) x y)) nt) m
        where
                nt = trans_mat n

cube_root n = last ([i|i<-[1..(n)],i*i*i<=n])
is_ram n = length [a| a<-[1..(cube_root n)], b<-[(a+1)..(cube_root n)], c<-[(a+1)..(cube_root n)], d<-[(c+1)..(cube_root n)], a*a*a + b*b*b == n && c*c*c + d*d*d == n] /= 0
nlist = [i| i<-[1..100000], is_ram i]

give_ram x 0 = x
give_ram x n = if is_ram x then give_ram x (n-1) else give_ram (x+1) n

ram n = give_ram 1700 n
        where
                give_ram x 0 = (x-1)
                give_ram x n = if is_ram x then give_ram (x+1) (n-1) else give_ram (x+1) n


cube_root n = floor (n ** (1/3))
is_ram n = length [a| a<-[1..crn], b<-[(a+1)..crn], c<-[(a+1)..crn], d<-[(c+1)..crn], a*a*a + b*b*b == n && c*c*c + d*d*d == n] /= 0
        where
                crn = floor (n ** (1/3))
-- nlist = [i| i<-[1..100000], is_ram i]

give_ram x 0 = x
give_ram x n = if is_ram x then give_ram x (n-1) else give_ram (x+1) n

ram n = give_ram 1 n
        where
                give_ram x 0 = (x-1)
                give_ram x n = if is_ram x then give_ram (x+1) (n-1) else give_ram (x+1) n


-- most likely final
merge [] bs = bs
merge as [] = as
merge as@(a:at) bs@(b:bt)
	= case compare a b of
	LT -> a : merge at bs
	EQ -> a : b : merge at bt
	GT -> b : merge as bt

cubes a = first ++ (rest `merge` cubes (a+1))
 where
	s = (a+1)^3 + (a+2)^3
	first = span (\x -> x < s) [a^3 + b^3 | b<- [(a+1)..]]

sameSum x y = x == y
rjgroups = groupBy sameSum $ cubes 1
rjnumbers = filter (\g -> length g > 1) rjgroups
ramanujan n = (init (last (take n rjnumbers)))!!0

is_matrix (x:[]) = (length x) /= 0
is_matrix [] = False
is_matrix (x:y:xs) = (length x) == (length y) && (is_matrix (y:xs))

is_square_matrix n = (is_matrix n) && ((length (n!!0)) == (length n))


addable m n = (is_matrix n) && (is_matrix m) && ((length n) == (length m)) && ((length (n!!0)) == (length (m!!0)))

add_matrix m n = if addable m n then zipWith (zipWith (+)) m n else [[]]

multiplyable m n = is_matrix m && is_matrix n && num_cols m == num_rows n
	where
		num_cols x = length (x!!0)
		num_rows x = length x

make_list m = map (\x -> (map (\y -> y:[]) x)) m
trans_mat m = foldl (\acc x -> zipWith (++) acc x) (head ls) (tail ls)
                where
                        ls = make_list m

multiply_matrix m n = if multiplyable m n then mat_mult m n else [[]]
	where
		mat_mult m n = map (\x -> map (\y -> foldl (+) 0 (zipWith (*) x y)) nt) m
        		where
		                nt = trans_mat n
