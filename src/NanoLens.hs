{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module NanoLens where

import Data.Monoid
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State.Class

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

infix 2 %=
infix 2 .=

(%=) :: MonadState s m => Traversal' s a -> (a -> a) -> m ()
l %= f = modify $ over l f

(.=) :: MonadState s m => Traversal' s a -> a -> m ()
l .= f = l %= const f

infixl 4 <$$>

-- | Flipped version of fmap. Useful for constructing lenses.
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
_right _ (Left x) = pure (Left x)

-- | Lens through two different lenses
throughBoth :: Traversal s t a b -> Traversal s' t' a b -> Traversal (s,s') (t,t') a b
throughBoth l1 l2 k (x,y) = (,) <$> l1 k x <*> l2 k y
