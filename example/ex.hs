{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Proxy
import           Elm
import           GHC.Generics
import           Data.Time.Calendar
import           Data.Time.Clock

data OnlyThree = One | Two | Three deriving (Show, Eq, Generic, ElmType)

data Person = Person
  { id :: Int
  , name :: Maybe String
  , birth :: Day
  , accountCreation :: UTCTime
  } deriving (Show, Eq, Generic, ElmType)

spec :: Spec
spec = moduleSpec ["Example"] $ do
  require "Date exposing (Date)"
  renderType (Proxy :: Proxy Person)
  renderDecoder (Proxy :: Proxy Person)
  renderEncoder (Proxy :: Proxy Person)
  renderType (Proxy :: Proxy OnlyThree)
  renderDecoder (Proxy :: Proxy OnlyThree)
  renderEncoder (Proxy :: Proxy OnlyThree)
  renderStringFrom (Proxy :: Proxy OnlyThree)
  renderStringFrom (Proxy :: Proxy (Maybe OnlyThree))
  renderStringTo (Proxy :: Proxy OnlyThree)
  renderStringTo (Proxy :: Proxy (Maybe OnlyThree))

main :: IO ()
main = specsToDir [spec] "src"
