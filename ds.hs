module Main where

-- 2. Persistence

-- 2.1 Suffixes
suffixes :: (Show a) => [a] -> [[a]]
suffixes [] = []
suffixes ls@(_:xs) = ls:(suffixes xs)

{-
  Original list:
    [0 A]  -> [1 B] -> [2 C]

  At each element, create a new list element pointing to beginning
  of list and point it to the rest of the list.

    [0 (pointer to 0')] [1 (pointer to 1')] [2 (pointer to 2')]

  which is n time and n space. I <3 immutability.
-}

-- BST

data Tree a =  Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq) 
-- type TreeInserter a = (Ord a) => Tree a -> a -> Tree a

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf val = Node val Leaf Leaf
insert t@(Node n l r) val
  | val > n = Node n l (insert r val)
  | val < n = Node n (insert l val) r
  | otherwise = t

member :: (Ord a) => Tree a -> a -> Bool
member Leaf _ = False                     
member (Node n l r) val
  | val > n = member r val
  | val < n = member l val
  | otherwise = True
                

member' :: (Ord a) => Tree a -> a -> Bool
member' t val = helper t Nothing
  where
    helper Leaf carry = carry == Just val
    helper (Node n l r) carry 
      | val <= n = helper l (Just n) 
      | otherwise = helper r carry 

-- These 2 are more efficient but feel ugly.

-- I'd rather avoid exceptions and prefer monads
insert' :: (Ord a) => Tree a -> a -> Tree a
insert' t val = case helper t of
    Just result -> result
    Nothing     -> t  
  where
    helper Leaf = Just $ Node val Leaf Leaf
    helper (Node n l r) 
      | val > n = case helper r of
          Just insertRight -> Just $ Node n l insertRight
          Nothing          -> Nothing
      | val < n = case helper l of
          Just insertLeft  -> Just $ Node n insertLeft r
          Nothing          -> Nothing
      | otherwise = Nothing

insert'' :: (Ord a) => Tree a -> a -> Tree a
insert'' t val = case helper t Nothing of
    Just result -> result
    Nothing     -> t  
  where
    helper Leaf carry
      | carry == Just val = Nothing
      | otherwise = Just $ Node val Leaf Leaf
    helper (Node n l r) carry
      | val <= n = case helper l (Just n) of
          Just insertLeft  -> Just $ Node n insertLeft r
          Nothing          -> Nothing
      | val > n = case helper r carry of
          Just insertRight -> Just $ Node n l insertRight
          Nothing          -> Nothing

    
-- Utilities

createTree :: (Ord a) => (Tree a -> a -> Tree a) -> [a] -> Tree a
createTree _      []     = Leaf
createTree ins (d:ds) = (flip ins) d $ createTree ins ds

printTree :: (Show a) => Tree a -> String
printTree t = helper [t]
  where
    printNode Leaf = ""
    printNode (Node n _ _) = show n

    helper []    = ""
    helper nodes = unwords (map printNode nodes) ++ "\n" ++
                   helper (children nodes)

    children [] = []
    children (Leaf:ns) = children ns
    children ((Node _ l r):ns) = l:r:(children ns)

assert :: String -> Bool -> String
assert test cond 
  | cond = "\x1b[1;32m[Pass]\x1b[0m " ++ test
  | otherwise = "\x1b[1;31m[Fail]\x1b[0m " ++ test

main :: IO ()
main = mapM_ putStrLn 
  [ assert "suffixes" $ suffixes ["A", "B"] == [["A", "B"], ["B"]]
  , assert "tree member exists" $ member sampleTree 'A'
  , assert "tree member doesn't exist" $ not $ member sampleTree '0'
  , assert "tree root member' exists" $ member' sampleTree 'B'
  , assert "tree member' doesn't exist" $ not $ member' sampleTree '0'
  , assert "testing smarter insert" $ sampleTree == sampleTree'
  , assert "testing smartest insert" $ sampleTree == sampleTree''
  ]
  where
    nodes = ['A', 'C' ..'K'] ++ ['B', 'D' .. 'K']
    sampleTree = createTree insert nodes
    sampleTree' = createTree insert' nodes
    sampleTree'' = createTree insert'' nodes
