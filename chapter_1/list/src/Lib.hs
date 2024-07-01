module Lib where

emptyList = []
prepend = 10 : []
list5 = 1 : 2 : 3 : 4 : 5 : []
list10 = [1..10]
infiniteList = [1..]
getHead = head [1..10]
getTail = tail [1..10]
allbutlast = init [1..10]
take10 = take 10 [1..]
drop10 = drop 10 [1..20]
get1331th = [1..] !! 1331
is10element = elem 10 [1..10]

isEmpty [] = True
isEmpty _ = False

isSize2 (x:y:[]) = True
isSize2 _ = False

cat2 = [1..10] ++ [11..20]

a2z = ['a'..'z']

strHead = head "abc"

zip2 = zip ['a'..'z'] [1..]

