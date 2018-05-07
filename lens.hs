{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

import BasePrelude
import Data.Functor.Identity

-- Sample data types

data Person = Person {
  name :: String,
  age  :: Int,
  address :: Address
  }
  deriving Show

data Address = Address {
  city :: String,
  country :: String }
  deriving Show

-- Example value

artyom :: Person
artyom = Person "Artyom" 22 (Address "Berlin" "Germany")

-- First-class fields

type Field a r =
  forall f. Functor f => (a -> f a) -> r -> f r

modify :: forall a r. Field a r -> (a -> a) -> r -> r
modify ra f r = runIdentity $ r_func r

  where
    -- We give this to 'modifyF'
    f' :: a -> Identity a
    f' = Identity . f

    -- This is the result of 'modifyF'
    r_func :: r -> Identity r
    r_func = ra f'

get :: forall a r. Field a r -> r -> a
get ra r = fst $ r_func r
  where
    f' :: a -> (,) a a
    f' a = (a, a)

    r_func :: r -> (,) a r
    r_func = ra f'

-- Definitions of 'Field's for all fields

namefield :: Field String Person
namefield = \f person ->
      (\l -> person {name = l}) <$> f (name person)

addressfield :: Field Address Person
addressfield = \f person ->
      (\l -> person {address = l}) <$> f (address person)

cityfield :: Field String Address
cityfield = \f address ->
      (\l -> address {city = l}) <$> f (city address)

-- Helpers
  
increment :: Num a => Field a r -> r -> r
increment = (+= 1)

(+=) :: Num a => Field a r -> a -> (r -> r)
(+=) field n = (modify field) (+n)

(%=) :: Field a r -> (a -> a) -> (r -> r)
(%=) = modify

(.=) :: Field a r -> a -> (r -> r)
(.=) field = modify field . const

infix 5 +=, %=, .=


  {-

-- Example of modification

oldArtyom = artyom
  & agefield += 3
  & namefield %= map toLower
  & addressfield.cityfield .= "Heidelberg"

-- Category instance

instance Category Field where
  id :: Field a a
  id = Field {
    get = id,
    modify = ($) }

  (.) :: Field b c -> Field a b -> Field a c
  (.) cb ba = Field {
    get = get ba . get cb,
    modify = modify cb . modify ba
  }

-}
