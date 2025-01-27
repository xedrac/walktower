module Equipment where

import Types qualified as T

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
  , stats :: T.BasicStats
  , slot :: Slot
  }
