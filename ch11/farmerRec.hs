module Farmer where

newtype Name = Name String deriving Show

newtype Acres = Acres Int deriving Show

-- FarmerType is a Sum
data FarmerType =
    DairyFarmer
    | WheatFarmer
    | SoybeanFarmer
    deriving Show

data FarmerRec =
    FarmerRec { name :: Name
              , acres :: Acres
              , farmerType :: FarmerType
              }
    deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = 
    case farmerType farmer of -- nice!
        DairyFarmer -> True
        _ -> False
