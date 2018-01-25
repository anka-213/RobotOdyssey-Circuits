{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module NanoLens where

import Data.Monoid
import Control.Applicative
import Control.Monad.Identity

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

over :: Traversal s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

view :: Lens' s a -> s -> a
view l = getConst . l Const

infixl 8 ^..

(^..) :: s -> Traversal' s a -> [a]
s ^.. l = toListOf l s

infixl 4 <$$>

(<$$>) :: Functor f => f (a -> b) -> a -> f b
f <$$> x = ($x) <$> f

toListOf :: Traversal' s a -> s -> [a]
toListOf l = foldrOf l (:) []

foldrOf :: Traversal' s a -> (a -> b -> b) -> b -> s -> b
foldrOf l f n = flip appEndo n . foldMapOf l (Endo . f)

foldMapOf :: Monoid r => Traversal' s a -> (a -> r) -> s -> r
foldMapOf l f = getConst . l (Const . f)

_right :: Traversal (Either l a) (Either l b) a b
_right k (Right x) = Right <$> k x
_right k (Left x) = pure (Left x)

-- | Lens through two different lenses
throughBoth :: Traversal s t a b -> Traversal s' t' a b -> Traversal (s,s') (t,t') a b
throughBoth l1 l2 k (x,y) = (,) <$> l1 k x <*> l2 k y
