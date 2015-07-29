import P1
import P2
import P3
import P4
import P5
import P6
import P7
import P8
import P9
import P10

testp1 = myLast [1,2,3,4] == 4 && myLast ['x','y','z'] == 'z'

testp2 = myButLast [1,2,3,4]==3 && myButLast['a'..'z']=='y'

testp3 = elementAt [1..3] 2 == 2 && elementAt "haskell" 5=='e'

testp4 = myLength [123,456,789] == 3 && myLength "Hello, world!" == 13

testp5 = myReverse "A man, a plan, a canal, panama!" == "!amanap ,lanac a ,nalp a ,nam A" && myReverse [1,2,3,4]==[4,3,2,1]

testp6 = isPalindrome [1,2,3] == False && isPalindrome "madamimadam" && isPalindrome [1,2,4,8,16,8,4,2,1]

p71 = flatten (Elem 5) == [5]
p72 = flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1,2,3,4,5]
p73 = length(flatten (List [])) == 0
testp7 = p71 && p72 && p73

testp8 = compress "aaaabccaadee" == "abcade" && compress "abcdef" == "abcdef"

testp9 = pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e'] == ["aaaa","b","cc","aa","d","eeee"]

testp10 = encode "aaaabccaadeeee" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

main :: IO ()
main = do
	print "Start testing"
	print (if testp1 then "Pass 1" else "Fail 1")
	print (if testp2 then "Pass 2" else "Fail 2")
	print (if testp3 then "Pass 3" else "Fail 3")
	print (if testp4 then "Pass 4" else "Fail 4")
	print (if testp5 then "Pass 5" else "Fail 5")
	print (if testp6 then "Pass 6" else "Fail 6")
	print (if testp7 then "Pass 7" else "Fail 7")
	print (if testp8 then "Pass 8" else "Fail 8")
	print (if testp9 then "Pass 9" else "Fail 9")
	print (if testp10 then "Pass 10" else "Fail 10")
