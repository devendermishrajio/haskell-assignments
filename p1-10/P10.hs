module P10
where
import P9
encode l = foldl (\x y -> x ++ [(length y, head y)]) [] (pack l)

