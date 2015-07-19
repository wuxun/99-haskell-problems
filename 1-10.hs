-- 1. (*) Find the last element of a list.
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' = head . reverse


-- 2. (*) Find the last but one element of a list.
myButLast [] = error "empty list"
myButLast [x] = error "list lenght less than 1"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

myButLast' = head . reverse . init

-- 3. (*) Find the K'th element of a list. The first element in the list is number 1.
elementAt (x:_) 1 = x
elementAt [] _ = error "Index out of bounds"
elementAt (_:xs) n
  | n > 1 = elementAt xs (n - 1)
  | otherwise = error "Index out of bounds"

elementAt' xs n = xs !! (n - 1)

-- 4. (*) Find the number of elements of a list.
myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc + 1) 0

myLength' :: [a] -> Int
myLength' list = length' list 0
  where length' [] n = n
        length' (x:xs) n = length' xs (n + 1)

-- 5. (*) Reverse a list.
myReverse :: [a] -> [a]
myReverse list = collect' list []
  where collect' [] acc_list = acc_list
        collect' (x:xs) acc_list = collect' xs (x:acc_list)

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- 6. (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = (reverse list) == list

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

-- 7. (**) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8. (**) Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress (x:ys@(y:_))
  | x == y = compress ys
  | otherwise = x : compress ys
compress ys = ys

-- 9. (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack list = pack' list [] []
  where pack' [] tmp result = reverse (tmp:result)
        pack' (x:xs) [] result = pack' xs [x] result
        pack' (x:xs) tmp@(y:_) result
          | x == y = pack' xs (x:tmp) result
          | otherwise = pack' xs [x] (tmp:result)

pack' (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest
pack' [] = []

-- 10. (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . pack
