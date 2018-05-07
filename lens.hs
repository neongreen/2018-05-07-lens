{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

type Field r r' a a' =
  forall f. Functor f => (a -> f a') -> r -> f r'

type Field' r a = Field r r a a
  -- forall f. Functor f => (a -> f a) -> r -> f r

modify
  :: forall a r a' r'.
     Field r r' a a' -> (a -> a') -> r -> r'
modify ra f r = runIdentity $ r_func r

  where
    -- We give this to 'modifyF'
    f' :: a -> Identity a'
    f' = Identity . f

    -- This is the result of 'modifyF'
    r_func :: r -> Identity r'
    r_func = ra f'

get :: forall a r a' r'. Field r r' a a' -> r -> a
get ra r = getConst $ r_func r
  where
    f' :: a -> Const a a'
    f' a = Const a

    r_func :: r -> Const a r'
    r_func = ra f'

-- Definitions of 'Field's for all fields

namefield :: Field' (Person e) String
namefield = \f person ->
      (\l -> person {name = l}) <$> f (name person)

addressfield :: Field' (Person e) Address
addressfield = \f person ->
      (\l -> person {address = l}) <$> f (address person)

cityfield :: Field' Address String
cityfield = \f address ->
      (\l -> address {city = l}) <$> f (city address)

-- Helpers
  
(+=) :: Num a => Field' r a -> a -> (r -> r)
(+=) field n = (modify field) (+n)

(%=) :: Field r r' a a' -> (a -> a') -> (r -> r')
(%=) = modify

(.=) :: Field r r' a a' -> a' -> (r -> r')
(.=) field = modify field . const

infix 5 +=, %=, .=

-- Example of modification

oldArtyom = artyom
  & namefield %= map toLower
  & addressfield.cityfield .= "Heidelberg"

  
extrafield :: Field (Person e) (Person e') e e'
extrafield = \f person -> 
      (\l -> person {extra = l}) <$> f (extra person)
