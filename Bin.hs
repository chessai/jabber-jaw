module Bin where

import Algebra

-- A binary tree is the fixed point of (1 + A*X*X)
data Node a b = Empty | Node a b b

-- Proof that (Node a) is an endofunctor
instance Functor (Node a) where
  fmap _ Empty = Empty
  fmap f (Node a b1 b2) = Node a (f b1) (f b2)

data Bin a = Tree { unTree :: Fix (Node a) }

empty :: Bin a
empty = Tree { unTree = initial Empty }

singleton :: a -> Bin a
singleton x = Tree { unTree = initial (Node x (initial Empty) (initial Empty)) }

makeTree :: a -> Bin a -> Bin a -> Bin a
makeTree x l r = Tree { unTree = initial $ Node x (unTree l) (unTree r) }

treeCatamorphism :: Algebra (Node a) x -> Bin a -> x
treeCatamorphism alg = catamorphism alg . unTree

treeAnamorphism :: CoAlgebra (Node a) x -> x -> Bin a
treeAnamorphism coalg = Tree . anamorphism coalg

nodeCase :: c -> (a -> b -> b -> c) -> Node a b -> c
nodeCase b _ Empty = b
nodeCase _ f (Node a b c) = f a b c

height :: Bin a -> Int
height = treeCatamorphism (nodeCase 0 (const $ (. succ) . max))

size :: Bin a -> Int
size = treeCatamorphism (nodeCase 0 (const $ (. succ) . (+)))
