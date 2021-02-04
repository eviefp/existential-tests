{-# language AllowAmbiguousTypes       #-}
{-# language DataKinds                 #-}
{-# language ExistentialQuantification #-}
{-# language KindSignatures            #-}
{-# language RankNTypes                #-}
{-# language RoleAnnotations           #-}
{-# language ScopedTypeVariables       #-}
{-# language TypeApplications          #-}

module Lib where

import Prelude
import Data.Data (cast, Typeable)


-- BackendType
data B = B1 | B2
  deriving Show

-- Backend class
class C (b :: B) where
instance C 'B1
instance C 'B2

-- parsing function
parse :: String -> Maybe B
parse ('1':_) = Just B1
parse ('2':_) = Just B2
parse _       = Nothing

-- SourceBackend b
data T (b :: B) = T String
  deriving Show

-- existential version
data T' = forall (b :: B). (C b, Typeable b) => T' (T b)

mkT' :: String -> T'
mkT' str = case parse str of
  Just b  -> case b of
    B1 -> T' @'B1 $ T str
    B2 -> T' @'B2 $ T str
  Nothing -> error "nope"

getT' :: T' -> Either (T 'B1) (T 'B2)
getT' (T' tb) = case cast tb of
  Just tb1 -> Left tb1
  Nothing -> case cast tb of
    Just tb2 -> Right tb2
    Nothing -> error "nope"

run :: IO ()
run = putStrLn "thing"
