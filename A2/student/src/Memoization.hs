module Memoization where

-- This is an assignment where we try to memoize functions
-- To speed up computations.

import Data.Maybe (fromJust)

-- The problem
-- If we write a recursive function, it may be slow
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- In GHCI, do,
-- > :set +s
-- (Will print the time it takes to run something)
-- > fibo 30
-- You'll see this runs slow
runSlow :: Int
runSlow = fibo 30

-- We can make it faster, by creating a cache
-- This takes a function and creates an infinite list of the results
-- (This works because Haskell doesn't evaluate things before we need them)
mkCache :: (Num a , Enum a ) => (a -> b) -> [b]
mkCache f = map f [0..]

-- And we make a function to look for a value in a cache
evalCache :: [a] -> Int -> a
evalCache c n = c !! n

-- We create a cache for the fibonacci function
-- Where we assume we have a fast function available
-- Notice how the cache doesn't know anything about the n we'll look for
-- This will be important later.
fiboCache :: [Int]
fiboCache = mkCache fastFibo1

-- Now, we write a function that looks up in the cache instead
-- (Cache is infinite so we should never get a miss...)
fastFibo1 :: Int -> Int
fastFibo1 0 = 0
fastFibo1 1 = 1
fastFibo1 n =
  let fib = evalCache fiboCache
  in fib (n-1) + fib (n-2)

-- We can see the cache growing by doing, in GHCI
-- > :print fiboCache -- Prints just 'fiboCache = (_t1::[Integer])' since the list isn't evaluated yet
-- > fastFibo1 10
-- > :print fiboCache
-- Prints the list, and we can see we just evaluated what needed it.
runFast :: Int
runFast = fastFibo1 30

-- We can make another cache, that works with keys and values
-- We pass the function and the domain of the keys as an argument
listCache :: [a] -> (a -> b) -> [(a, b)]
listCache domain f = map (\a -> (a, f a)) domain

-- We create a function which looks up the
-- result in the cache
-- and use fromJust to get an error if the cache misses.
listLookup :: Eq a => [(a, b)] -> a -> b
{- TO BE WRITTEN -}
listLookup cache value = fromJust $ lookup value cache

-- Create the cache for all integers...
-- We use a 'fast fibonacci function' even if we haven't defined it yet!
fibCache :: [(Int, Int)]
{- TO BE WRITTEN -}
fibCache = listCache [0..] fastFibo2

-- And the fast function looks in the cache!
fastFibo2 :: Int -> Int
fastFibo2 n = fromJust $ lookup n fibCache
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
-- Pause:
-- We make the solution in two parts:
-- 1. We have a cache that uses a 'fast function' we don't have yet (but we know it's type!)
-- 2. We have a fast function that looks for the result in the cache
-- This wouldn't work in other languages, but because of lazy evaluation, it works, magic!

-- Using criterion, we can see that the fastFib function is a little slower than fasterFibo.
-- by using "stack bench --benchmark-arguments="--output bench.html"
-- And looking at the plots.

-- Now, for something cool
-- What if we make a function that creates the cache, and immediately looks in it?
memoizeWithList :: Eq a => [a] -> (a -> b) -> (a -> b)
memoizeWithList domain = listLookup . listCache domain

-- Maybe we can use it to memoize the old fibo function?
testMemoize :: Int -> Int
testMemoize n =
  let fibo2 = memoizeWithList [0..] fibo
  in fibo2 n

-- It doesn't work... because the old fibo function calls itself
-- So even if we use memoize, the recursive calls don't use the cache...

-- To fix this, we need open recursion.
-- It looks tricky, but it's simple
-- 1. Look at the recursive function
-- 2. Replace the function calls to calls to another function
-- 3. And his 'other function' is a function f, passed as a parameter.

-- Here is Fibonacci in open recursion
-- This function can do other things than compute fibonacci, but
-- It isn't really recursive anymore
-- And it's easy to implement fibonacci again: (openFib fibo) does that.
openFib :: (Int -> Int) -> Int -> Int
{- TO BE WRITTEN -}
openFib _ 0 = 0
openFib _ 1 = 1
openFib f n = f (n-1) + f (n-2)

-- We use openFib to create a cached function, and make sure
-- The recursive calls call the fast version!
fastFibo3 :: Int -> Int
fastFibo3 = memoizeWithList [0..] (openFib fastFibo3)

-- The memoize function creates the cache and looks in it immediately
-- And because of Lazy evaluation, we get a function that takes a slow
-- version, and returns a fast version.

-- LONGEST PALINDROMIC SUBSEQUENCE

-- Utility
dropLast :: [a] -> [a]
dropLast l = take (length l - 1) l

-- Slow version
lps :: String -> String
lps s
  | length s <= 1 = s
  | otherwise = if head s == last s then head s : lps (dropLast (tail s)) ++ [last s]
  else if length a > length b then a else b
    where
      a = lps (dropLast s)
      b = lps (tail s)

-- >>> lps "sesbaese"
-- "esese"


-- CACHES FOR LISTS OF THINGS

-- If we want caches for lists, it's more complicated...
-- Having a list of all possible lists will not work
-- (Cause we meet an infinity of strings like 'a', 'aa', 'aaaaaa'
-- when we look up)
-- Instead, we need a trie
-- A Trie node edge will store a value for the node, and edges
-- to its children, where each label has type 'edge'
-- 'node' and 'edge' can be any types.
data Trie node edge = Trie node [(edge, Trie node edge)]
  deriving Show

-- First, looking for a list in a trie...
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

-- To build an infinite trie, we start from the root
-- The root starts with the empty list...
-- And from that, we have a number of edges
-- The domain 'dom' defines how many edges we have per node
rootTrie :: [a] -> Trie [a] a
{- TO BE WRITTEN -}
rootTrie domain = Trie [] (edges domain [])

-- How do we create the edges?
-- We look at the domain,
-- and create an edge for each value, with:
-- 1. a label
-- 2. a subtree build with
-- the domain, the current label
-- and the current node
edges :: [a] -> [a] -> [(a, Trie [a] a)]
edges domain currentNode = map (\e -> (e, subtree domain e currentNode)) domain

-- How do we build the subtree?
-- We use the label we just followed
-- and the parent node
-- And each child creates more edges!
-- (using the edges function)
subtree :: [a] -> a -> [a] -> Trie [a] a
{- TO BE WRITTEN -}
subtree domain label parent = Trie curr (edges domain curr)
  where curr = parent ++ [label]

-- Important: the trie is infinite because edges calls subtree, and subtree calls edges.

-- trieCache builds a cache for a function
-- provided with a domain (for the list elements)
trieCache :: [e] -> ([e] -> b) -> Trie b e
trieCache domain function = mapTrie function (rootTrie domain)

{--
You can inspect the cache with GHCI!

> let cache = trieCache ['a' .. 'z'] reverse
> :print cache

Prints the cache, and we can see nothing is evaluated
(Warning: Make sure function doesn't return a generic type, otherwise it doesn't work)

> trieLookup cache []
> :print cache

Prints the cache, and also the unevaluated thunks.
Print is very useful when you want to debug calculations with infinite structures
Cause it doesn't evaluate the whole thing.

And then, you can do

> trieLookup cache "hej"
> :print cache

Prints the cache, and you should be able to see it has grown
--}

testTrie :: [Char] -> [Char]
testTrie =
  let cache = mapTrie reverse $ rootTrie ['a'..'z']
  in trieLookup cache

-- We can test it on the LPS function
-- Computes the "longest palyndromic subsequence"
-- So, The length of the sequence of characters so that:
-- It's a palyndrome
-- It contains characters of the input (but you can skip some)
-- (Is slow)

-- First, some test strings
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

-- >>> (fastLPS s2)
-- Maybe.fromJust: Nothing



-- openLPS :: (String -> String) -> (String -> String)
-- openLPS sf = undefined -- look at 'lps' for inspiration

psTrieCache :: Trie String Char
psTrieCache = trieCache ['a'..'z'] fastLPS
-- use trieLookup to find the Node in the trie
-- each node is represented with a string, with the root node being an empty string
-- each edge is the character that comes next in the string.
-- We should therefore in openLPS, when we are at a node, if the first and last are the same char, (palindromic)
  -- we find lps of the substring without first and last
--   fastLPS is involved somehow to get the results of the trimmed strings
--   When we reach palindromes or empty lists for all, we compare lengths to
--     get theLPS


openLPS :: (String -> String) -> (String -> String)
openLPS flps s
  | length s <= 1 = s
  | otherwise = if head s == last s then head s : flps (dropLast (tail s)) ++ [last s]
  else if length a > length b then a else b
      where
        a = flps (dropLast s)
        b = flps (tail s)


-- creates a function that uses open recursion. Applies the function sf at each
-- step, and recurses itself
-- should recurse to all edges in the trie from the current node
-- then compare lengths when done. Longest is the one returned

-- lps :: String -> String
-- lps s
  -- | length s <= 1 = s
  -- | otherwise = if head s == last s then head s : lps (dropLast (tail s)) ++ [last s]
  -- else if length a > length b then a else b
    -- where
      -- a = lps (dropLast s)
      -- b = lps (tail s)



-- Fast!
fastLPS :: String -> String
fastLPS = openLPS $ trieLookup psTrieCache

-- >>> fastLPS "arstoien"
-- "n"

-- So, what were the tricks?
-- The first one is to build an infinite data-structure, to memoize the function
-- And then your function looks in the cache for the answer!
