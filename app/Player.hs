module Player where

data Player = Player
  { name :: String
  , stats :: BasicStats
  , traits :: Traits
  } deriving (Show, Eq)

-- Traits that can be trained to improve base stats
data Traits = Traits
  { strength :: Int     -- Power  (lifting weights)
  , vitality :: Int     -- Health and constitution (Walking, Cardio, BMI)
  , agility :: Int      -- Flexibility, nimbleness (Stretching, Knees over Toes guy, BMI)
  , intelligence :: Int -- Learning, Prayer, Scripture Study
  } deriving (Show, Eq)

