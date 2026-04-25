module Memoization where

import Data.Maybe (fromJust)

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

runSlow :: Int
runSlow = fibo 30

mkCache :: (Num a , Enum a ) => (a -> b) -> [b]
mkCache f = map f [0..]

evalCache :: [a] -> Int -> a
evalCache c n = c !! n

fiboCache :: [Int]
fiboCache = mkCache fastFibo1

fastFibo1 :: Int -> Int
fastFibo1 0 = 0
fastFibo1 1 = 1
fastFibo1 n =
  let fib = evalCache fiboCache
  in fib (n-1) + fib (n-2)

runFast :: Int
runFast = fastFibo1 30

listCache :: [a] -> (a -> b) -> [(a, b)]
listCache domain f = map (\a -> (a, f a)) domain

listLookup :: Eq a => [(a, b)] -> a -> b
listLookup cache value = fromJust $ lookup value cache

fibCache :: [(Int, Int)]
fibCache = listCache [0..] fastFibo2

-- And the fast function looks in the cache!
fastFibo2 :: Int -> Int
fastFibo2 0 = 0
fastFibo2 1 = 1
fastFibo2 n = listLookup fibCache (n-1) + listLookup fibCache (n-2)
-- the first time we run it, the cache is empty, and in the first test we use
-- the old fibo function, therefore this should be slower since we also store
-- and look stuff up. If we run it on the same input again it is a lot faster.
-- even faster once we use fastFibo to generate the cache as well.
-- ghci> fastFibo2 20
-- 6765
-- (0.02 secs, 4,552,328 bytes)
-- ghci> fibo 20
-- 6765
-- (0.01 secs, 4,542,760 bytes)
-- ghci> fastFibo2 20
-- 6765
-- (0.00 secs, 515,080 bytes)

memoizeList :: Eq a => [a] -> (a -> b) -> (a -> b)
memoizeList domain = listLookup.listCache domain

memoizeWithList :: Eq a => [a] -> (a -> b) -> (a -> b)
memoizeWithList domain = listLookup . listCache domain

testMemoize :: Int -> Int
testMemoize n =
  let fibo2 = memoizeWithList [0..] fibo
  in fibo2 n


openFib :: (Int -> Int) -> Int -> Int
openFib _ 0 = 0
openFib _ 1 = 1
openFib f n = f (n-1) + f (n-2)

fastFibo3 :: Int -> Int
fastFibo3 = memoizeWithList [0..] (openFib fastFibo3)

-- LONGEST PALINDROMIC SUBSEQUENCE

dropLast :: [a] -> [a]
dropLast l = take (length l - 1) l

-- Slow version

{- Wrote this version first, and then refactored to the one below
lps :: String -> String
lps s
  | length s <= 1 = s
  | otherwise = if head s == last s then head s : lps (dropLast (tail s)) ++ [last s]
  else if length a > length b then a else b
    where
      a = lps (dropLast s)
      b = lps (tail s)
-}

lps :: String -> String
lps [] = []
lps [a] = [a]
lps (x:xs)
  | x == lx = x : lps dlxs ++ [lx]
  | length r1 > length r2 = r1
  | otherwise = r2
    where
      dlxs = dropLast xs
      lx = last xs
      r1 = lps (x:dlxs)
      r2 = lps xs



-- >>> lps "sesbaese"
-- "esese"

data Trie node edge = Trie node [(edge, Trie node edge)]
  deriving Show

trieLookup :: Eq e => Trie a e -> [e] -> a
-- empty list at current node -> return node
trieLookup (Trie a _) [] = a
trieLookup (Trie _ e) l = trieLookup (fromJust (lookup (head l) e)) (tail l)

-- get edges for a
-- take the edge that matches the next item in the list
-- recurse on that
hiho :: Trie Integer Char
hiho = Trie 0 [
   ('h', Trie 1 [('i', Trie 2 []),
                 ('o', Trie 2 [('o', Trie 3 [])])])
   ]
-- >>> trieLookup hiho "hoo"
-- 3
-- >>> trieLookup hiho "hi"
-- 2

limitTrie :: Int -> Trie n e -> Trie n e
limitTrie 0 (Trie v _) = Trie v []
limitTrie n (Trie v es) =
  Trie v [(l, limitTrie (n-1) t) | (l, t) <- es]

-- Map a function over all values in the trie
-- Edge labels stay the same.
-- example of open recursion. We recurse with a function provided upon call.
-- apply the function to the value at the current node, and create a new trie with that
-- add all edges, and perform maptrie on those tries recursively
mapTrie :: (a -> b) -> Trie a e -> Trie b e
mapTrie f (Trie v cs) = Trie (f v) (map (\(e, t) -> (e, mapTrie f t)) cs)

-- >>> mapTrie (\a -> a+2) hiho
-- Trie 2 [('h',Trie 3 [('i',Trie 4 []),('o',Trie 4 [('o',Trie 5 [])])])]

rootTrie :: [a] -> Trie [a] a
rootTrie domain = Trie [] (edges domain [])

edges :: [a] -> [a] -> [(a, Trie [a] a)]
edges domain currentNode = map (\e -> (e, subtree domain e currentNode)) domain

subtree :: [a] -> a -> [a] -> Trie [a] a
subtree domain label parent = Trie curr (edges domain curr)
  where curr = parent ++ [label]

trieCache :: [e] -> ([e] -> b) -> Trie b e
trieCache domain function = mapTrie function (rootTrie domain)

testTrie :: [Char] -> [Char]
testTrie =
  let cache = mapTrie reverse $ rootTrie ['a'..'z']
  in trieLookup cache


k1 :: String
k1 = "writers"
k2 :: String
k2 = "vintner"
l1 :: String
l1 = "aferociousmonadatemyhamster"
l2 :: String
l2 = "functionalprogrammingrules"
s1 :: String
s1 = "bananrepubliksinvasionsarmestabsadjutant"
s2 :: String
s2 = "kontrabasfiolfodralmakarmästarlärling"

-- >>> (fastLPS k1)
-- >>> (fastLPS k2)
-- >>> (fastLPS l1)
-- >>> (fastLPS l2)
-- >>> (fastLPS s1)
-- >>> (fastLPS s2)
-- "rer"
-- "ntn"
-- "rsmahamsr"
-- "uniargrainu"
-- "naubsasnsasbuan"
-- "nilrlmakamlrlin"

psTrieCache :: Trie String Char
psTrieCache = trieCache (['a'..'z'] ++ ['å', 'ä', 'ö']) fastLPS
-- use trieLookup to find the Node in the trie
-- each node is represented with a string, with the root node being an empty string
-- each edge is the character that comes next in the string.
-- We should therefore in openLPS, when we are at a node, if the first and last are the same char, (palindromic)
  -- we find lps of the substring without first and last
--   fastLPS is involved somehow to get the results of the trimmed strings
--   When we reach palindromes or empty lists for all, we compare lengths to
--     get theLPS

{- Wrote this version first, and then refactored to the one below
openLPS :: (String -> String) -> (String -> String)
openLPS flps s
  | length s <= 1 = s
  | otherwise = if head s == last s then head s : flps (dropLast (tail s)) ++ [last s]
  else if length a > length b then a else b
      where
        a = flps (dropLast s)
        b = flps (tail s)
-}
openLPS :: (String -> String) -> (String -> String)
openLPS _ [] = []
openLPS _ [a] = [a]
openLPS flps (x:xs)
  | x == lx = x : flps dlxs ++ [lx]
  | length r1 > length r2 = r1
  | otherwise = r2
    where
      dlxs = dropLast xs
      lx = last xs
      r1 = flps (x:dlxs)
      r2 = flps xs

fastLPS :: String -> String
fastLPS = openLPS $ trieLookup psTrieCache

-- >>> fastLPS "banananas"
-- "ananana"
