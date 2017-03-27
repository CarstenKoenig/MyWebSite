{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleInstances #-}

module Models.Projections
  ( Projection
  , getResult
  , liftP
  , lastP, collectP, projectP
  , (<<*), (<<$)
  )
where

import Data.Maybe (catMaybes)
import Data.List (foldl')

data Projection event state result =
  Projection
  { foldP :: state -> event -> state
  , resultP :: state -> result
  , initP :: state
  }


getResult :: Projection event state result -> [event] -> result
getResult p =
    resultP p . foldl' (foldP p) (initP p)



liftP :: (e1 -> Maybe e0)
      -> Projection e0 s r -> Projection e1 s r
liftP proj p =
  Projection fP (resultP p) (initP p)
  where
    fP s ev =
      case proj ev of
        Just ev' -> foldP p s ev'
        Nothing  -> s


projectP :: (event -> b) -> Projection event [b] [b]
projectP p = Projection update reverse []
  where
    update xs ev = p ev : xs


collectP :: (event -> Maybe a) -> Projection event [Maybe a] [a]
collectP select = catMaybes <$> projectP select


lastP :: (event -> Maybe a) -> Projection event (Maybe a) (Maybe a)
lastP select = Projection update id Nothing
  where
    update cur ev = select ev `orElse` cur
    a@(Just _) `orElse` _ = a
    Nothing `orElse` b = b


----------------------------------------------------------------------
-- Operators

infixl 8 <<*
f <<* pX = constant f `apply` pX

infixl 7 <<$
pF <<$ pX = pF `apply` pX


----------------------------------------------------------------------
-- Helpers

constant :: a -> Projection event () a
constant a = Projection const (const a) ()


apply :: Projection event state (a -> b)
      -> Projection event state' a
      -> Projection event (state,state') b
apply pf pa =
  uncurry ($) <$> parallel pf pa


parallel :: Projection event state result
         -> Projection event state' result'
         -> Projection event (state, state') (result, result')
parallel p1 p2 =
  Projection fP rP iP
  where
    iP = (initP p1, initP p2)
    rP (s1,s2) = (resultP p1 s1, resultP p2 s2)
    fP (s1,s2) ev = (foldP p1 s1 ev, foldP p2 s2 ev)
    

----------------------------------------------------------------------
-- Instances

instance Functor (Projection event state) where
  fmap f (Projection fP rP iP) =
    Projection fP (f . rP) iP
