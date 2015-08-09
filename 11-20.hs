-- 11. (*) Modified run-length encoding.
data Compress a = Single a | Multiple Int a
     deriving (Show)

pack' :: (Eq a) => [a] -> [[a]]
pack' (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack' rest
pack' [] = []

encodeModified :: (Eq a) => [a] -> [Compress a]
encodeModified = map (\x -> if (length x == 1) then Single (head x) else Multiple (length x) (head x)) . pack'

-- 12. (**) Decode a run-length encoded list.
decodeModified :: [Compress a] -> [a]
decodeModified = concatMap decodeHelper
  where
    decodeHelper (Single x) = [x]
    decodeHelper (Multiple n x) = replicate n x

-- 13. (**) Run-length encoding of a list (direct solution).
encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = foldr encodeHelper []
  where
    encodeHelper x [] = [(1, x)]
    encodeHelper x (y@(a,b):ys)
      | x == b = (a+1, b):ys
      | otherwise = (1, x):y:ys

encodeDirect :: (Eq a) => [a] -> [Compress a]
encodeDirect = map helper . encode'
  where
    helper (1, x) = Single x
    helper (n, x) = Multiple n x

-- 14. (*) Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- 15. (**) Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli list n = concatMap (replicate n) list

-- 16. (**) Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery list n = dropEvery' list n n
  where
    dropEvery' [] _ _ = []
    dropEvery' (x:xs) i n
              | i == 1 = dropEvery' xs n n
              | otherwise = x : dropEvery' xs (n - 1) n

-- 17. (*) Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split = flip splitAt

-- 18. (**) Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice list start end = take (end - start + 1) $ drop (start - 1)list

-- 19. (**) Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate list n
  | n > 0 = drop n list ++ take n list
  | otherwise = let count = n + length list
                in drop count list ++ take count list

-- 20. (*) Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt n list = helper n list []
  where
    helper n (x:xs) head
      | n == 1 = (x, head ++ xs)
      | otherwise = helper (n - 1) xs (head ++ [x])
