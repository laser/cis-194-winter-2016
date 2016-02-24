
addingIsFun :: [Int] -> Int
addingIsFun [] = 0
addingIsFun (x:xs) = x + addingIsFun xs

addingIsFun [1,2,3]
1 + addingIsFun [2,3]
1 + 2 + addingIsFun[3]
1 + 2 + 3 + addingIsFun[]
1 + 2 + 3 + 0
3 + 3 + 0
6 + 0
6

--expressions are only evaluated when pattern matched!

data List a = Nil |  Cons a (List a)

data Stream a = Cons a (Stream a)

--does this work?
data Stream a = a : (Stream a)

-- Cons is more general; (:) assumes second argument is a list

addNForever :: Stream Int -> Int -> Stream Int
addNForever (Cons x xs) n = Cons (x + n) (addNForever xs n)

addNList :: List Int -> Int -> List Int
addNList [] _ = []
addNList (x:xs) n = (x + n) : (addNList xs n)
