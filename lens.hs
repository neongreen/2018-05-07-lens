{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

import BasePrelude

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

data Field r a = Field {
  get :: r -> a,
  modify :: (a -> a) -> r -> r }

-- Definitions of 'Field's for all fields

namefield :: Field Person String
namefield = Field {
  get = \person -> name person,
  modify = \f person -> person {name = f (name person)}
  }

agefield :: Field Person Int
agefield = Field {
  get = \person -> age person,
  modify = \f person -> person { age = f (age person) }
  }

addressfield :: Field Person Address
addressfield = Field {
  get = \person -> address person,
  modify = \f person -> person { address = f (address person) }
  }

cityfield :: Field Address String
cityfield = Field {
  get = \address -> city address,
  modify = \f address -> address { city = f (city address) }
  }

countryfield :: Field Address String
countryfield = Field {
  get = \address -> country address,
  modify = \f address -> address { country = f (country address) }
  }

-- Helpers
  
increment :: Num a => Field r a -> r -> r
increment = (+= 1)

(+=) :: Num a => Field r a -> a -> (r -> r)
(+=) field n = (modify field) (+n)

(%=) :: Field r a -> (a -> a) -> (r -> r)
(%=) = modify

(.=) :: Field r a -> a -> (r -> r)
(.=) field = modify field . const

-- Example of modification

oldArtyom = artyom
  & agefield += 3
  & namefield %= map toLower
--  & addressfield.cityaddress .= "Heidelberg"

-- Unfinished Category instance

instance Category Field where
  id :: Field a a
  id = Field {
    get = id,
    modify = ($) }

  (.) :: Field b c -> Field a b -> Field a c
  (.) bc ab = Field {
    get = get bc . get ab,
    modify a = modify 
