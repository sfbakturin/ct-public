module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

--
-- @author Saveliy Bakturin
-- <p>
-- Don't write off, if you don't wanna be banned!
--

data Meta = M Int Int
  deriving (Show)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                      = 0
tsize (Branch (M size _) _ _ _) = size

tHeight :: Tree a -> Int
tHeight Leaf                        = 0
tHeight (Branch (M _ height) _ _ _) = height

mkTree :: a -> Tree a
mkTree e = mkBranch Leaf e Leaf

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch = Branch (M 1 1)

tRotateLeft :: Tree a -> Tree a
tRotateLeft Leaf = Leaf
tRotateLeft (Branch (M _ _) Leaf root Leaf) = mkTree root
tRotateLeft (Branch (M size height) (Branch (M lsize lheight) lleft lroot lright) root Leaf)
  = tRecalcHeightSize (Branch (M size height) (tRecalcHeightSize (Branch (M lsize lheight) lleft lroot lright)) root Leaf)
tRotateLeft (Branch (M size height) left root (Branch (M rsize rheight) rleft rroot rright))
  = tRecalcHeightSize (Branch (M size rheight) (tRecalcHeightSize (Branch (M rsize height) left root rleft)) rroot rright)

tRotateRight :: Tree a -> Tree a
tRotateRight Leaf = Leaf
tRotateRight (Branch (M size height) Leaf root Leaf) = Branch (M size height) Leaf root Leaf
tRotateRight (Branch (M size height) Leaf root (Branch (M rsize rheight) rleft rroot rright))
  = tRecalcHeightSize (Branch (M size height) Leaf root (tRecalcHeightSize (Branch (M rsize rheight) rleft rroot rright)))
tRotateRight (Branch (M size height) (Branch (M lsize lheight) lleft lroot lright) root right)
  = tRecalcHeightSize (Branch (M size lheight) lleft lroot (tRecalcHeightSize (Branch (M lsize height) lright root right)))

tBigRotateLeft :: Tree a -> Tree a
tBigRotateLeft Leaf = Leaf
tBigRotateLeft (Branch (M size height) left root right) = tRotateLeft (Branch (M size height) left root (tRotateRight right))

tBigRotateRight :: Tree a -> Tree a
tBigRotateRight Leaf = Leaf
tBigRotateRight (Branch (M size height) left root right) = tRotateRight (Branch (M size height) (tRotateLeft left) root right)

tRecalcHeightSize :: Tree a -> Tree a
tRecalcHeightSize Leaf = Leaf
tRecalcHeightSize (Branch (M _ _) Leaf root Leaf) = mkTree root
tRecalcHeightSize (Branch (M _ _) (Branch (M lsize lheight) lleft lroot lright) root Leaf)
  = Branch (M (lsize + 1) (lheight + 1)) (Branch (M lsize lheight) lleft lroot lright) root Leaf
tRecalcHeightSize (Branch (M _ _) Leaf root (Branch (M rsize rheight) rleft rroot rright))
  = Branch (M (rsize + 1) (rheight + 1)) Leaf root (Branch (M rsize rheight) rleft rroot rright)
tRecalcHeightSize (Branch (M _ _) (Branch (M lsize lheight) lleft lroot lright) root (Branch (M rsize rheight) rleft rroot rright))
  = Branch (M (lsize + rsize + 1) (max lheight rheight + 1)) (Branch (M lsize lheight) lleft lroot lright) root (Branch (M rsize rheight) rleft rroot rright)

tdepth :: Tree a -> Int
tdepth = tHeight

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember e (Branch _ left root right)
  | e == root = True
  | e < root = tmember e left
  | otherwise = tmember e right

tLeft :: Tree a -> Tree a
tLeft Leaf                      = Leaf
tLeft (Branch (M _ _) left _ _) = left

tRight :: Tree a -> Tree a
tRight Leaf                       = Leaf
tRight (Branch (M _ _) _ _ right) = right

tBalanceDiffTest :: Tree a -> Bool
tBalanceDiffTest Leaf = False
tBalanceDiffTest (Branch (M _ _) left _ right)
  | tHeight left > tHeight right = True
  | otherwise = False

tBalanceLeft :: Tree a -> Tree a
tBalanceLeft t
  | tBalanceDiffTest (tRight t) = tBigRotateLeft t
  | otherwise = tRotateLeft t

tBalanceRight :: Tree a -> Tree a
tBalanceRight t
  | not (tBalanceDiffTest (tLeft t)) = tBigRotateRight t
  | otherwise = tRotateRight t

tBalance :: Tree a -> Tree a
tBalance Leaf = Leaf
tBalance (Branch (M size height) left root right)
  | tHeight right >= tHeight left + 2 = tBalanceLeft (Branch (M size height) left root right)
  | tHeight left >= tHeight right + 2 = tBalanceRight (Branch (M size height) left root right)
  | otherwise = Branch (M size height) left root right

tinsertImpl :: Ord a => a -> Tree a -> Tree a
tinsertImpl e Leaf = mkTree e
tinsertImpl e (Branch (M size height) left root right)
  | e < root = tBalance (tRecalcHeightSize (Branch (M (size + 1) height) (tRecalcHeightSize (tinsertImpl e left)) root right))
  | otherwise = tBalance (tRecalcHeightSize (Branch (M (size + 1) height) left root (tRecalcHeightSize (tinsertImpl e right))))

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert e Leaf = mkTree e
tinsert e t
  | tmember e t = t
  | otherwise = tinsertImpl e t

tFromListImpl :: Ord a => [a] -> Tree a -> Tree a
tFromListImpl xs root = foldl (flip tinsert) root xs

tFromList :: Ord a => [a] -> Tree a
tFromList []     = Leaf
tFromList (x:xs) = tFromListImpl xs (mkTree x)
