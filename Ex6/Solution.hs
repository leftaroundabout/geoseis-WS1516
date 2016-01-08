{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances, UndecidableInstances #-}

module Ex6.Solution where

import LaTeX.GeoSeisExercise

import Numeric.AD
import Numeric.GSL.Fitting

import Control.Monad


main :: ExerciseSheet
main = mkSolutionSheet $ do
   taskNo 6 "" $ do
      "For earthquake 1, we find the following fault plane parameters:"
      forM_ [eq1] $ \eq -> do
         " strike"
         mathDisplay' $ phiu =: showAngle (eqFaultStrike eq)
         " dip"
         mathDisplay' $ delta =: showAngle (eqFaultDip eq)
         " second nodal plane with"
         newline
      
      




data Earthquake = Earthquake {
      eqFaultStrike :: Uncertain Azimuth
    , eqFaultDip :: Uncertain Dip
    , eqFP2Strike :: Uncertain Azimuth
    , eqFP2Dip :: Uncertain Dip
    , eqTAzimuth :: Uncertain Azimuth
    , eqTPlunge :: Uncertain Angle
    , eqPAzimuth :: Uncertain Azimuth
    , eqPPlunge :: Uncertain Angle
    , eqSlipλ :: Uncertain Angle
    }

eq1 :: Earthquake
eq1 = q { eqSlipλ = ((274.5 :± 5)°R) - eqFaultStrike q }
 where q = Earthquake {
        eqFaultStrike = (40 :± 9)°R
      , eqFaultDip = (61 :± 4)°R
      , eqFP2Strike = (4 :± 5)°R
      , eqFP2Dip = (33 :± 5)°R
      , eqTAzimuth = 296°R
      , eqTPlunge = 16°R
      , eqPAzimuth = 164°R
      , eqPPlunge = 68°R
      , eqSlipλ = 0
      }
         
                             


