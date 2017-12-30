module Algebra where

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a

-- Fix :: (* -> *) -> *
-- The fixed point of a functor must be an initial aglebra, if it is defined
-- note that `type Fix f = f (Fix f)` constructs an infinite type, and is not permissible.
data Fix f = C (f (Fix f))

-- (->), combines an f (Fix f) into a Fix f
initial :: (Functor f) => Algebra f (Fix f)
initial = C

-- (<-), deconstruction 
unInitial :: Fix f -> f (Fix f)
unInitial (C f) = f

coInitial :: Fix f -> f (Fix f)
coInitial (C f) = f

coUnInitial :: (Functor f) => Algebra f (Fix f)
coUnInitial = C

-- A morphism `f` between Algebra f a and Algebra f b is any
-- function f :: a -> b, satisfying
-- f . Algebra f a = Algebra f b . fmap f
type AlgebraMorphism   a b = a -> b
type CoAlgebraMorphism a b = b -> a

-- A catamorphism is the unique morphism from an initial algebra to some other algebra
-- |^ = -> . |^ . <-
catamorphism :: (Functor f) => Algebra f x -> AlgebraMorphism (Fix f) x
catamorphism alg = alg . fmap (catamorphism alg) . unInitial

-- An anamorphism is the categorical dual of a catamorphism
anamorphism :: (Functor f) => CoAlgebra f x -> CoAlgebraMorphism (Fix f) x
anamorphism alg = coUnInitial . fmap (anamorphism alg) . alg

-- A hylomorphism is the composition of a catamorphism and an anamorphism.
hylomorphism :: (Functor f) => Algebra f y -> CoAlgebra f x -> x -> y
hylomorphism alg coalg = catamorphism alg . anamorphism coalg
