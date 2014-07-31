-- Code excerpted from The Monad Reader, Issue 14 "Fun with Morse Code" by Heinrich Apfelmus
-- A Haskell newbie's annotations added to document confusion/illumination along the way

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