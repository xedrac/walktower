module WalkTower.Equipment where

import WalkTower.Types

-- Location that a piece of equipment is worn
data Slot
  = SlotHead
  | SlotChest
  | SlotHands
  | SlotWaist
  | SlotLegs
  | SlotFeet
  | SlotWeapon

-- A piece of equipment that can be worn or wielded
data Equipment = Equipment
  { name :: String
  , stats :: BasicStats
  , slot :: Slot
  }
