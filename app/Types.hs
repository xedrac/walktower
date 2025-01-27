module Types where

-- Basic stats of a player or piece of equipment
data BasicStats = BasicStats
  { level :: Int
  , health :: Int
  , defense :: Int
  , attack :: Int
  }


