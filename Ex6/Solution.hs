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
      "For earthquake 1, we find the following fault plane parameters: "
      " (the uncertainties are obtained from measuring the spread to equivalent"
      " fault-plane models, except for the T- and P axis azimuth and plunge, whose"
      " uncertainties are only rough estimates)"
      let eqInfo eq = do
           " strike"
           mathDisplay' $ phiu =: showAngle (eqFaultStrike eq)
           " dip"
           mathDisplay' $ delta =: showAngle (eqFaultDip eq)
           " second nodal plane with strike"
           mathDisplay $ phiu =: showAngle (eqFP2Strike eq)
           " and dip"
           mathDisplay' $ delta =: showAngle (eqFP2Dip eq)
           " T-axis with azimuth"
           mathDisplay $ showAngle (eqTAzimuth eq)
           " and plunge"
           mathDisplay' $ showAngle (eqTPlunge eq)
           " P-axis with azimuth"
           mathDisplay $ showAngle (eqPAzimuth eq)
           " and plunge"
           mathDisplay'' $ showAngle (eqPPlunge eq)
           "The slip angle is"
           mathDisplay' $ lambda =: showAngle (eqSlipλ eq)
           let faultKinds = [ (0°R,  "left lateral strike-slip")
                            , (90°R, "thrust fault")
                            , (180°R,"right lateral strike slip")
                            , (90°L, "normal fault")
                            ]
               faultKByLikely
                  = snd <$> sortBy ( comparing $
                                      abs . angleDifference (expected $ eqSlipλ eq) . fst )
                                   faultKinds
           "which corresponds best to a ">>emph(fromString $ head faultKByLikely)
           ", followed by ">>emph(fromString $ faultKByLikely!!1)>>"."
           newline
      eqInfo eq1
      "Here, ">>math lambda>>" was determined from the (uncertain) absolute direction"
      " of the slip vector, minus the fault-plane azimuth, with the usual uncertainty"
      " propagation."
      
      




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

eq1, eq2 :: Earthquake

eq1 = q { eqSlipλ = ((274.5 :± 5)°R) - eqFaultStrike q }
 where q = Earthquake {
        eqFaultStrike = (40 :± 9)°R
      , eqFaultDip = (61 :± 4)°R
      , eqFP2Strike = (4 :± 5)°R
      , eqFP2Dip = (33 :± 5)°R
      , eqTAzimuth = (296 :± 5)°R
      , eqTPlunge = (16 :± 5)°R
      , eqPAzimuth = (164 :± 5)°R
      , eqPPlunge = (68 :± 5)°R
      , eqSlipλ = 0
      }
         
eq2 = q { eqSlipλ = ((274.5 :± 5)°R) - eqFaultStrike q }
 where q = Earthquake {
        eqFaultStrike = (10 :± 2)°R
      , eqFaultDip = (79 :± 2)°R
      , eqFP2Strike = (96 :± 3)°R
      , eqFP2Dip = (80 :± 2)°R
      , eqTAzimuth = (296 :± 5)°R
      , eqTPlunge = (16 :± 5)°R
      , eqPAzimuth = (164 :± 5)°R
      , eqPPlunge = (68 :± 5)°R
      , eqSlipλ = 0
      }
         
                             


