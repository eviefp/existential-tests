{-# language AllowAmbiguousTypes       #-}
{-# language DataKinds                 #-}
{-# language GADTs #-}
{-# language KindSignatures            #-}
{-# language RankNTypes                #-}
{-# language RoleAnnotations           #-}
{-# language ScopedTypeVariables       #-}
{-# language TypeApplications          #-}

module Lib2 where

import Prelude


-- BackendType
data B = B1 | B2
  deriving Show

----------------------------------------------
-- Singleton stuff
data SB (b :: B) where
  SB1 :: SB 'B1
  SB2 :: SB 'B2
----------------------------------------------

-- Backend class
class C (b :: B) where
  sb :: SB b -- this would be in the SingI singleton instance but we can pack it here
  f :: T b -> String
instance C 'B1 where
  sb = SB1
  f (T s) = s
instance C 'B2 where
  sb = SB2
  f (T s) = s

-- parsing function
parse :: String -> Maybe B
parse ('1':_) = Just B1
parse ('2':_) = Just B2
parse _       = Nothing

-- SourceBackend b
data T (b :: B) = T String
  deriving Show

-- existential version
data T' where
  T' :: C b => T b -> T'

mkT' :: String -> T'
mkT' str = case parse str of
  Just b_  -> case b_ of
    B1 -> T' @'B1 $ T str
    B2 -> T' @'B2 $ T str
  Nothing -> error "nope"

getT' :: T' -> Either (T 'B1) (T 'B2)
getT' (T' (t :: T b)) = case (sb :: SB b) of
  SB1 -> Left t
  SB2 -> Right t

lift :: T' -> (forall b. C b => T b -> r) -> r
lift (T' t) k = k t

run :: IO ()
run = putStrLn "thing"
