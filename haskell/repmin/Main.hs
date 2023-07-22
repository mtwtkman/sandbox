module Main where

data Tree a = Leaf a | Branch (Tree a) (Tree a)

repmin_2pass :: Ord a => Tree a -> Tree a
repmin_2pass t =
  let globalmin = findmin t
   in rep globalmin t
  where
    findmin (Leaf x) = x
    findmin (Branch l r) = min (findmin l) (findmin r)

    rep x (Leaf _) = Leaf x
    rep x (Branch l r) = Branch (rep x l) (rep x r)

repmin_1pass :: Ord a => Tree a -> Tree a
repmin_1pass t = t'
  where
    (t', globalmin) = repmin t
    repmin (Leaf x) = (Leaf globalmin, x)
    repmin (Branch l r) =
      (Branch l' r', min lmin rmin)
      where
        (l', lmin) = repmin l
        (r', rmin) = repmin r

main :: IO ()
main = return ()
