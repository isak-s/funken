-- Erik Åstrand(er8863as-s) Isak Simonsson(is1746si-s)

module Chatterbot where
  
import Utilities
import System.Random
import Data.Char
import Data.Maybe

-- If you're not sure what this is, it's ok.
import Control.Monad (mapM)

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
{- TO BE WRITTEN -}
makePair (Rule (patt, temp) ) = do
  rand <- randomIO ::  IO Float
  return (patt, pick rand temp)

rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply list = try $ transformationsApply reflect list

-- >>> rulesApply [(stringToPattern "*" "I hate *", stringToPattern "*" "Why do you hate * ?")] (words "I hate my mother") 
-- ["Why","do","you","hate","your","mother","?"]


-- >>> reflect ["i", "will", "never", "see", "my", "reflection", "in", "your", "eyes"]
-- ["you","will","never","see","your","reflection","in","my","eyes"]

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = map (try (`lookup` reflections)) 
-- map (\t -> try (\x -> lookup x reflections) t)  

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
{- TO BE WRITTEN -}
ruleCompile (patt, temp) =  Rule ( starPattern (map toLower patt), map starPattern temp)   

--------------------------------------


-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- mkPattern '*' "Hi *!" => [Item 'H', Item 'i', Wildcard, Item '!']
mkPattern :: Eq a => a -> [a] -> Pattern a
{- TO BE WRITTEN -}
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

reductionsApply :: [(Pattern String, Pattern String)] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply pat = fix (try (transformationsApply id pat)) 

-- >>> prepare "can you please tell me what Haskell is"
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

-- >>> substitute (mkPattern 'x' "3*cos(x) + 4 - x") "5.37"
-- "3*cos(5.37) + 4 - 5.37"

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => Pattern a -> [a] -> Maybe [a]
{- TO BE WRITTEN -}


-- empty list, empty pattern or only wildcards. Needed for if a string ends in a wildcard
match (Pattern patt) [] = if all (== Wildcard) patt then Just [] else Nothing


-- >>> match (mkPattern 'x' "abcd") "abcd"
-- Just ""

-- No wildcard in the pattern
match (Pattern patt) xs 
  |Wildcard `notElem` patt =
    if patt == map Item xs then Just [] else Nothing

-- Starts with wildcard
match (Pattern (Wildcard:ps)) xs = orElse
  (singleWildcardMatch (Pattern (Wildcard:ps)) xs)
  (longerWildcardMatch (Pattern (Wildcard:ps)) xs)

-- Starts with normal item
match (Pattern (Item p : ps)) (x:xs)
  | p ==  x   = match (Pattern ps) xs
  | otherwise = Nothing

singleWildcardMatch, longerWildcardMatch :: Eq a => Pattern a -> [a] -> Maybe [a]
singleWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  mmap (const [x]) (match (Pattern ps) xs)
longerWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  mmap (x:) (match (Pattern  (Wildcard:ps)) xs)

-- >>> singleWildcardMatch (mkPattern '*' "*do") "bdo"


-- >>> singleWildcardMatch (mkPattern '*' "*do") "dobedo"


-- >>> singleWildcardMatch (mkPattern '*' "*do") "bedobe"


-- >>> longerWildcardMatch (mkPattern '*' "*do") "bdo"


-- >>> longerWildcardMatch (mkPattern '*' "*do") "dobedo"
-- Just "dobe"

-- >>> longerWildcardMatch (mkPattern '*' "*do") "bedobe"


-- >>> match (mkPattern '*' "*do")"bdo"


-- >>> match (mkPattern '*' "*do")"dobedo"


-- >>> match (mkPattern '*' "*do")"bedobe"




-- >>> match (mkPattern '*' "frodo") "gandalf"
-- >>> match (mkPattern 'x' "2*x+3+x") "2*7+3"
-- >>> match (mkPattern 'x' "abcd") "abcd"
-- >>> match (mkPattern 2 [1,3..5]) [1,3..5]
-- >>> match (mkPattern 'x' "2*x+3") "2*7+3"
-- >>> match (mkPattern '*' "* and *") "you and me"


-------------------------------------------------------
-- Applying patterns transformations
--------------------------------------------------------

-- Helper function: Matches a pattern and applies the transformation
matchAndTransform :: Eq a => ([a] -> [a]) -> Pattern a -> [a] -> Maybe [a]
matchAndTransform transform pat = (mmap transform) . (match pat)


-- >>> transformationApply id "My name is Zacharias" (mkPattern '*' "My name is *", mkPattern '*' "Je m'appelle *") 
-- Just "Je m'appelle Zacharias"

-- Applying a single pattern
transformationApply :: Eq a => ([a] -> [a]) -> [a] -> (Pattern a, Template a) -> Maybe [a]
{- TO BE WRITTEN -}
transformationApply f xs (patt, temp) = mmap (substitute temp) (matchAndTransform f patt xs) 

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
transformationsApply f [] xs = Nothing
transformationsApply f (ps:listPatt) xs = orElse (transformationApply f xs ps) (transformationsApply f listPatt xs)
  