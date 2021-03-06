{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}

import BasePrelude
import Data.Functor.Const
import Data.Functor.Identity

-- Sample data types

data Person e = Person {
  name :: String,
  age  :: Int,
  address :: Address,
  extra :: e
  }
  deriving Show

data Address = Address {
  city :: String,
  country :: String }
  deriving Show

-- Example value

artyom :: Person ()
artyom = Person "Artyom" 22 (Address "Berlin" "Germany") ()

-- First-class fields

type Optic f r r' a a' = (a -> f a') -> r -> f r'

type Lens r r' a a' =
  forall f. Functor f => Optic f r r' a a'

type Lens' r a = Lens r r a a
  -- forall f. Functor f => (a -> f a) -> r -> f r

type Traversal r r' a a' =
  forall f. Applicative f => Optic f r r' a a'

type Traversal' r a = Traversal r r a a
  -- forall f. Functor f => (a -> f a) -> r -> f r

both :: Traversal (a, a) (a', a') a a'
both f (a1, a2) = (,) <$> f a1 <*> f a2

-- f :: a -> f a'
each :: Traversal [a] [a'] a a'
each f []     = pure []
each f (x:xs) = (:) <$> f x <*> each f xs

modify
  :: forall a r a' r'.
     Optic Identity r r' a a' -> (a -> a') -> r -> r'
modify ra f r = runIdentity $ r_func r

  where
    -- We give this to 'modifyF'
    f' :: a -> Identity a'
    f' = Identity . f

    -- This is the result of 'modifyF'
    r_func :: r -> Identity r'
    r_func = ra f'

get :: forall a r a' r'. Optic (Const a) r r' a a' -> r -> a
get ra r = getConst $ r_func r
  where
    f' :: a -> Const a a'
    f' a = Const a

    r_func :: r -> Const a r'
    r_func = ra f'

data ConstList x a = ConstList {un :: [x]}
  deriving (Functor)

instance Applicative (ConstList x) where
  pure _ = ConstList []
  (<*>) (ConstList a) (ConstList b) = ConstList (a <> b)

toListOf :: Optic (ConstList a) r r' a a' -> r -> [a]
toListOf o r = un $ o (\a -> ConstList [a]) r

filtered :: (a -> Bool) -> Traversal' a a
filtered p f a
  | p a = f a
  | otherwise = pure a

-- Definitions of 'Lens's for all fields

namefield :: Lens' (Person e) String
namefield = \f person ->
      (\l -> person {name = l}) <$> f (name person)

addressfield :: Lens' (Person e) Address
addressfield = \f person ->
      (\l -> person {address = l}) <$> f (address person)

cityfield :: Lens' Address String
cityfield = \f address ->
      (\l -> address {city = l}) <$> f (city address)

_1 :: Optic Identity (a, x) (b, x) a b
_1 f (a, x) = (,x) <$> f a

-- Helpers
  
(+=) :: Num a => Optic Identity r r a a -> a -> (r -> r)
(+=) field n = (modify field) (+n)

(%=) :: Optic Identity r r' a a' -> (a -> a') -> (r -> r')
(%=) = modify

(.=) :: Lens r r' a a' -> a' -> (r -> r')
(.=) field = modify field . const

infix 5 +=, %=, .=

-- Example of modification

oldArtyom = artyom
  & namefield %= map toLower
  & addressfield.cityfield .= "Heidelberg"

  
extrafield :: Lens (Person e) (Person e') e e'
extrafield = \f person -> 
      (\l -> person {extra = l}) <$> f (extra person)
