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

data Field a r = Field {
  modifyF :: forall f. Functor f => (a -> f a) -> r -> f r }

modify :: forall a r. Field a r -> (a -> a) -> r -> r
modify ra f r = runIdentity $ r_func r

  where
    -- We give this to 'modifyF'
    f' :: a -> Identity a
    f' = Identity . f

    -- This is the result of 'modifyF'
    r_func :: r -> Identity r
    r_func = (modifyF ra) f'

get :: forall a r. Field a r -> r -> a
get ra r = fst $ r_func r
  where
    f' :: a -> (,) a a
    f' a = (a, a)

    r_func :: r -> (,) a r
    r_func = (modifyF ra) f'

-- Definitions of 'Field's for all fields


{-

namefield :: Field String Person
namefield = Field {
  get = \person -> name person,
  modify = \f person -> person {name = f (name person)}
  }

agefield :: Field Int Person 
agefield = Field {
  get = \person -> age person,
  modify = \f person -> person { age = f (age person) }
  }

addressfield :: Field Address Person
addressfield = Field {
  get = \person -> address person,
  modify = \f person -> person { address = f (address person) }
  }

cityfield :: Field String Address
cityfield = Field {
  get = \address -> city address,
  modify = \f address -> address { city = f (city address) }
  }

countryfield :: Field String Address
countryfield = Field {
  get = \address -> country address,
  modify = \f address -> address { country = f (country address) }
  }

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
