module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

-- If you're not sure what this is, it's ok.
import Control.Monad (mapM)
import Control.Arrow (Arrow(first))
import Data.Foldable (find)
import Data.List.NonEmpty (prependList)

-- A pattern is a list of things
-- Where we have either a value or a wildcard
data PatternElem a = Wildcard | Item a
  deriving (Eq, Show)

-- A pattern is a list of pattern elements
newtype Pattern a = Pattern [PatternElem a]
  deriving (Eq, Show)

-- Templates are the same as patterns
type Template a = Pattern a

-- A phrase is a list of string
type Phrase = [String]

newtype Rule = Rule (Pattern String, [Template String])
  deriving (Eq, Show)

type BotBrain = [Rule]

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()


--------------------------------------------------------

-- This takes a brain, and returns a function
-- Which will take a phrase as an input and calculate the result
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind b =
  fmap rulesApply (mapM makePair b)

-- A rule maps a pattern to many answers, so we choose one
-- at random, and that's our bot
makePair :: Rule -> IO (Pattern String, Template String)
makePair (Rule (patt, templates)) =
  do
    rand <- randomRIO (0, 1) :: IO Double
    return (patt, pick rand templates)


rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
rulesApply l = try $ transformationsApply reflect l

testRulesApply = (stringToPattern "*" "changed *", stringToPattern "*" "It did in fact change *")
-- >>> rulesApply [testRulesApply] (words "changed you")
-- ["It","did","in","fact","change","me"]

reflect :: Phrase -> Phrase
reflect = map swap
  where
    swap w = case find (\ref -> fst ref == w) reflections of
      Just (_, replacement) -> replacement
      Nothing -> w
-- >>> reflect ["i", "am", "penis", "you"]
-- ["you","are","penis","me"]

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map ruleCompile

ruleCompile :: (String, [String]) -> Rule
ruleCompile (patt, temps) = Rule (starPattern (map toLower patt), map starPattern temps)

--------------------------------------


-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- mkPattern '*' "Hi *!" => [Item 'H', Item 'i', Wildcard, Item '!']
mkPattern :: Eq a => a -> [a] -> Pattern a
mkPattern a = Pattern . map (\t -> if t == a then Wildcard else Item t)

-- >>> mkPattern '*' "Hi *!"
-- Pattern [Item 'H',Item 'i',Item ' ',Wildcard,Item '!']

stringToPattern :: String -> String -> Pattern String
stringToPattern wc = mkPattern wc . words

starPattern :: String -> Pattern String
starPattern = stringToPattern "*"

reductions :: [(Pattern String, Pattern String)]
reductions = (map . map2) (starPattern, starPattern)
  [ ( "please *", "*" ),
    ( "could you *", "*" ),
    ( "can you *", "*"),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions
-- transformationsApply :: Eq a => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
reductionsApply :: [(Pattern String, Pattern String)] -> Phrase -> Phrase
reductionsApply redions p
  | p == reducedP = p
  | otherwise = reductionsApply redions reducedP
  where Just reducedP = orElse (transformationsApply id redions p) (Just p)

-- map over reductions and create a contains function for each

-- >>> reduce (prepare "can you please tell me what Haskell is")
-- ["what","is","haskell"]

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a template with the list given as the third argument
substitute :: Eq a => Template a -> [a] -> [a]
substitute (Pattern a) b = concatMap aux a
  where
    aux Wildcard = b
    aux (Item a) = [a]

--- >>> substitute (mkPattern 'x' "3*cos(x) + 4 - x") "5.37"
-- "3*cos(5.37) + 4 - 5.37"

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => Pattern a -> [a] -> Maybe [a]

match (Pattern []) [] = Just []
match (Pattern []) _ = Nothing
match (Pattern patt) [] =
  if all (== Wildcard) patt then Just [] else Nothing

-- No wildcard
match (Pattern patt) xs
  | Wildcard `notElem` patt =
      if patt == map Item xs then Just [] else Nothing

-- Starts with wildcard
match (Pattern (Wildcard:ps)) xs =
  case singleWildcardMatch (Pattern (Wildcard:ps)) xs of
    Just res -> Just res
    Nothing  -> longerWildcardMatch (Pattern (Wildcard:ps)) xs

-- Starts with normal item
match (Pattern (Item p : ps)) (x:xs)
  | p ==  x   = match (Pattern ps) xs
  | otherwise = Nothing

singleWildcardMatch, longerWildcardMatch :: Eq a => Pattern a -> [a] -> Maybe [a]
singleWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  orElse (fmap (const [x]) (match (Pattern ps) xs)) Nothing

longerWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  orElse (fmap (x:) (match (Pattern  (Wildcard:ps)) xs)) Nothing

-------------------------------------------------------
-- Applying patterns transformations
--------------------------------------------------------

-- Helper function: Matches a pattern and applies the transformation
matchAndTransform :: Eq a => ([a] -> [a]) -> Pattern a -> [a] -> Maybe [a]
matchAndTransform transform pat = (mmap transform) . (match pat)

-- Applying a single pattern
transformationApply :: Eq a => ([a] -> [a]) -> [a] -> (Pattern a, Template a) -> Maybe [a]
transformationApply f s (patt, temp) = mmap (substitute temp) (matchAndTransform f patt s)
-- mmap (substitute temp) (mmap f (match patt s))

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
transformationsApply f [] s = Nothing
transformationsApply f ((patt, temp):xs) s
  | isJust res = res
  | otherwise  = transformationsApply f xs s
  where res = transformationApply f s (patt, temp)
