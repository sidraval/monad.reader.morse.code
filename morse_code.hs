-- Code excerpted from The Monad Reader, Issue 14 "Fun with Morse Code" by Heinrich Apfelmus
-- A Haskell newbie's annotations added to document confusion/illumination along the way

type MorseCode = String

-- Dichotomic Search (http://en.wikipedia.org/wiki/Dichotomic_search)

-- Type constructor Tree
data Tree a = Leaf | Branch {
                              tag   :: a,
                              left  :: Tree a,
                              right :: Tree a
                            } deriving (Show)

-- A hand typed (abbreviated) version of the morse code tree mentioned in the above wikipedia page
dictByHand :: Tree Char
dictByHand = Branch ' '
                  (Branch 'E'
                    (Branch 'I' (Leaf) (Leaf))
                    (Branch 'A' (Leaf) (Leaf))
                  )
                  (Branch 'T'
                    (Branch 'N' (Leaf) (Leaf))
                    (Branch 'M' (Leaf) (Leaf))
                  )

-- Takes a single morse code letter and decodes it according to the above tree
-- Recurse over the string and choose our branches based on the current character
simpleDecodeLetter :: String -> Char
simpleDecodeLetter = tag . foldl (flip step) dictByHand
--                                (flip step) because we want to feed the step function
--                                the dictionary/accumulator before the character
                     where
                       step '.' = left
                       step '-' = right

-- Typing the whole morse code tree out as above would totally suck, so let's encode it
-- in a string. Leaf nodes will correspond to '_', while branches will correspond to their character.
stringEncodedDict = "__5__4H___3VS__F___2 UI__L__+_ R__P___1JWAE" ++ "__6__=B__/_XD__C__YKN__7_Z__QG__8_ __9__0 OMT "

-- Generates the dictionary by feeding string characters into their correspond constructor functions
-- and gathering intermediary trees in an array along the way. The head of the resultant array will
-- be the full tree. Then, the above simpleDecodeLetter will work by replacing dictByHand with generatedDict
generatedDict :: Tree Char
generatedDict = interpret stringEncodedDict
  where
    interpret = head . foldl exec []
    exec xs '_' = Leaf : xs
    exec (x:y:zs) c = Branch c x y : zs

-- We can do a similar thing with a call tree graph, where each node is a function that calls down to other
-- nodes (functions) depending on the character being read in.

-- Naive way: The E node becomes the following function...
e :: MorseCode -> Char
e ('.':ds) = i ds
e ('-':ds) = a ds
e []       = 'E'
-- where the functions i and a are defined similarly. Obviously defining all of these functions would suck.
-- In the interest of making this compile,
a = undefined
i = undefined

-- A combinator!
branch :: Char
          -> (MorseCode -> Char)
          -> (MorseCode -> Char)
          -> (MorseCode -> Char)
-- The branch returns a function, since x and y are functions/nodes in the call graph.
branch c x y = \code -> case code of
  '.':ds -> x ds
  '-':ds -> y ds
  []     -> c

-- The leaf function should never be called.
leaf = undefined

-- We can then generate our tree using the combinator as follows.
-- By taking the head, we're getting a function that goes from MorseCode -> Char, i.e.
-- the function that decodes a letter!
combinatorDict :: MorseCode -> Char
combinatorDict = interpret stringEncodedDict
  where
    interpret = head . foldl exec []
    exec xs '_' = leaf : xs -- leaf is just the undefined function, since it should never be called
    exec (x:y:zs) c = branch c x y : zs -- branch c x y is the function for the c node.
