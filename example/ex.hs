{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Proxy
import qualified Data.Map                      as Map
import           Elm
import           GHC.Generics
import           Data.Time.Calendar
import           Data.Time.Clock

data OnlyThree = One | Two | Three deriving (Show, Eq, Generic, ElmType)

data Pairs = FirstPair String | SecondPair String Int deriving (Show, Eq, Generic, ElmType)

data OfficeRef = ExistingOffice Int | NewOffice Int OnlyThree deriving (Show, Eq, Generic, ElmType)

data Person = Person
  { id :: Int
  , name :: Maybe String
  , birth :: Day
  , accountCreation :: UTCTime
  } deriving (Show, Eq, Generic, ElmType)


data Building = Building
  { address :: String
  , floors :: OfficeRef
  , person :: Person
  } deriving (Show, Eq, Generic, ElmType)

data MappyMcMapFace = MappyMcMapFace {  lMap :: (Map.Map String Int),
                                            adjList :: [(Int, Int)],
                                            rMap :: (Map.Map String Int) } deriving (Eq, Show, Generic, ElmType)

spec :: Spec
spec = moduleSpec ["Example"] $ do
  require "Date exposing (Date)"
  require "Fuzz exposing (..)"
  renderType (Proxy :: Proxy Person)
  renderDecoder (Proxy :: Proxy Person)
  renderEncoder (Proxy :: Proxy Person)
  renderFuzz (Proxy :: Proxy Person)
  renderDummy (Proxy :: Proxy Person)
  renderType (Proxy :: Proxy OnlyThree)
  renderFuzz (Proxy :: Proxy OnlyThree)
  renderDummy (Proxy :: Proxy OnlyThree)
  renderDecoder (Proxy :: Proxy OnlyThree)
  renderEncoder (Proxy :: Proxy OnlyThree)
  renderStringFrom (Proxy :: Proxy OnlyThree)
  renderStringTo (Proxy :: Proxy (Maybe OnlyThree))
  renderType (Proxy :: Proxy OfficeRef)
  renderDecoder (Proxy :: Proxy OfficeRef)
  renderEncoder (Proxy :: Proxy OfficeRef)
  renderFuzz (Proxy :: Proxy OfficeRef)
  renderDummy (Proxy :: Proxy OfficeRef)
  renderType (Proxy :: Proxy Building)
  renderFuzz (Proxy :: Proxy Building)
  renderType (Proxy :: Proxy Pairs)
  renderFuzz (Proxy :: Proxy Pairs)
  renderDummy (Proxy :: Proxy Pairs)
  renderDecoder (Proxy :: Proxy Pairs)
  renderEncoder (Proxy :: Proxy Pairs)
  renderType (Proxy :: Proxy MappyMcMapFace)
  renderFuzz (Proxy :: Proxy MappyMcMapFace)
  renderDummy (Proxy :: Proxy MappyMcMapFace)

main :: IO ()
main = specsToDir [spec] "src"
