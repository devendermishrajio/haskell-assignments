import P11
testp11 = encodeModified "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
testp12 = decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] == "aaaabccaadeeee"

testp14 = dupli [1,2,3] == [1,1,2,2,3,3] && dupli "abccd" == "aabbccccdd"

testp15 = repli "abc" 3 == "aaabbbccc" && repli [1,2,3] 3 == [1,1,1,2,2,2,3,3,3]

testp16 = dropEvery "abcdefghik" 3 == "abdeghk" && dropEvery "abcdefghik" 2 == "acegi"

testp17 = split "abcdefghik" 3 == ("abc", "defghik")

testp18 = slice "abcdefghik" 3 7 == "cdefg"

testp19 = rotate "abcdefgh" 3 == "defghabc" && rotate "abcdefgh" (-2) == "ghabcdef"

testp20 = removeAt 2 "abcd" == ('b',"acd") && removeAt 3 "abcdefghik" == ('c', "abdefghik")

main :: IO ()
main = do
	print "Start testing"
	print (if testp11 then "Pass 11" else "Fail 11")
	print (if testp12 then "Pass 12" else "Fail 12")
	print (if testp14 then "Pass 14" else "Fail 14")
	print (if testp15 then "Pass 15" else "Fail 15")
	print (if testp16 then "Pass 16" else "Fail 16")
	print (if testp17 then "Pass 17" else "Fail 17")
	print (if testp18 then "Pass 18" else "Fail 18")
	print (if testp19 then "Pass 19" else "Fail 19")
	print (if testp20 then "Pass 20" else "Fail 20")
