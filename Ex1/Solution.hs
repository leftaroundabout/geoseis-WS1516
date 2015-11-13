{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances, UndecidableInstances #-}

module Ex1.Solution where

import LaTeX.GeoSeisExercise
import Control.Monad


main :: ExerciseSheet
main = mkSolutionSheet $ do
   "Given is the following data from GPS stations near the San Andreas Fault: west of the fault (on the Pacific Plate),"
   lnbk
   displayGPSStations gpsStationsWest
   lnbk
   "and east of it (North American Plate):"
   lnbk
   displayGPSStations gpsStationsEast
   lnbk
   taskNo 1 "" $ do
      "The two station nearest to the fault, on either side, seem to be POSO and TWR2. "
      let Just poso = lookup "POSO" gpsStationsWest
          Just twr2 = lookup "TWR2" gpsStationsEast
      "The rate of slippage between two such stations can be calculated as"
      mathDisplay $ "v"!|"c" =: "v"!:(dblPipe<>2) - "v"!:(dblPipe<>1)
      "with uncertainty"
      mathDisplay $ sigma<>"v"!|"c" =: tsqrt Nothing (
                   ( sigma<>"v"!:(dblPipe<>1))^:2 + (sigma<>"v"!:(dblPipe<>2))^:2 )
                   <> "."
      "For these two stations, that comes out as"
      let CounterSlip v vU = sr_2stations twr2 poso
      mathDisplay $ "v"!|"c|POSO,TWR2" =: (v ± vU) <> physU(tfrac"mm""yr")
      "With such a large uncertainty, this is hardly a useful value."
   taskNo 3 "" $ do
      "We ignore the individual uncertainties of the stations, and"
      " take the average of all stations west, and east of the fault respectively,"
      " each with uncertainty from the standard deviation."
      [vWest, vEast] <- forM [(gpsStationsWest, "west"), (gpsStationsEast, "east")]
                        $ \(sta, dir) -> do
          let slip@(AbsSlip spa spaU _ _) = gpsStations_Average sta
          mathDisplay $ "v"!|dir =: (spa ± spaU) <> physU(tfrac"mm""yr")
          return slip
      "The slip difference between these is"
      let CounterSlip v vU = sr_2stations vEast vWest
      mathDisplay $ "v"!|"ca" =: (v ± vU) <> physU(tfrac"mm""yr")
   taskNo 5 "" $ do
      "In the following use a slip rate of "
      let vSchmӓlzle = 36; vSchmU = 2
      mathDisplay $ "v"!|"c" =: "v"!|"Schmlzl"
                             =: (vSchmӓlzle ± vSchmU) *|: tfrac"mm""yr"
      let avgRec = 205  :: Floating n => n
      "Over the average recurrence interval for Californian earthquakes of "
      math avgRec >> " yr, "
      "this amounts to a displacement of"
      let dA = vSchmӓlzle * avgRec / 1000
      mathDisplay $ "d"!|"A" =: (avgRec*|:"yr") `cdot` "v"!|"c"
                             =: (dA ± vSchmU * avgRec / 1000) *|: "m"
      "The uncertainty is not really meaningful here, since the recurrence rate is"
      " probably not so accurately known; we will ignore it in the following."
      lnbk
      "Given is an empirical relation between rupture length of various faults on earth,"
      " and the corresponding slip displacement per event."
      let logDisplc0 = -1.43
          logDisplcSlope = 0.88
          e = exp 1
      mathDisplay $ logBase 10 ("d"!|"A" / physU"m")
                       =: realNum logDisplc0
                          + realNum logDisplcSlope `cdot` logBase 10 ("l"!|"SR" / physU"km")
      "Inverting this relation yields"
      let logRupllen0 = - logDisplc0 * logRupllenSlope
          logRupllenSlope = recip logDisplcSlope
      mathDisplay $ logBase 10 ("l"!|"SR" / physU"km")
                       =: realNum logRupllen0
                           + realNum logRupllenSlope `cdot` logBase 10 ("d"!|"A" / physU"m")
      "which gives an estimate for the surface rupture length of the San Andreas Fault:"
      let ruptlen = 10**(logRupllen0 + logRupllenSlope * logBase 10 dA) :: Double
      mathDisplay $ "l"!|"SR" =: fromString (printf "%.2g" ruptlen) *|: "km"
      "Also given:"
      let momMag0 = 5.08
          momMagSlope = 1.16
      mathDisplay $ "M" =: realNum momMag0
                           + realNum momMagSlope `cdot` logBase 10 ("l"!|"SR")
      "Which gives"
      let momMag = momMag0 + momMagSlope * logBase 10 ruptlen
      mathDisplay $ "M" =: fromString (printf "%.2g" momMag)<>"."
      
      



displayGPSStations :: forall m . (Monad m) => GPStations -> LaTeXT m ()
displayGPSStations stats =
   tabular Nothing (replicate 3 CenterColumn) $ do
      "Site" &: "Fault ||" &: ("Fault "<>math bot)
      lnbk<>hline
      "Name" &: "Rate [mm/yr]" &: "Rate [mm/yr]"
      lnbk<>hline
      forM_ stats $ \(nm, AbsSlip spa spaU spe speU) -> do
          fromString nm &: math(fromString(show spa) +- fromString(show spaU))
                        &: math(fromString(show spe) +- fromString(show speU))
          lnbk
         
                             


data AbsSlip = AbsSlip {
      slipParl, spaUncert, slipPerp, speUncert :: ℝ
     } deriving (Show)

type GPStations = [(String, AbsSlip)]

gpsStationsWest, gpsStationsEast :: GPStations
gpsStationsWest = [ ("A504", AbsSlip 37.5 0.6 3.0 0.4)
                  , ("A505", AbsSlip 42.9 1.4 4.1 0.5)
                  , ("LNDA", AbsSlip 42.5 0.5 4.4 0.5)
                  , ("SAL2", AbsSlip 36.0 0.6 1.6 0.4)
                  , ("POSO", AbsSlip 32.0 1.2 0.9 0.3) ]
gpsStationsEast = [ ("CAUV", AbsSlip 25.8 0.5 2.9 0.5)
                  , ("GOUD", AbsSlip 23.4 0.3 1.9 0.1)
                  , ("LASY", AbsSlip 29.3 0.4 2.5 0.3)
                  , ("NAPO", AbsSlip 25.4 0.8 3.9 0.7)
                  , ("P807", AbsSlip 19.8 0.5 3.4 0.5)
                  , ("TWR2", AbsSlip 29.7 0.8 3.5 0.4) ]

data CounterSlip = CounterSlip { counterSlip, csUncert :: ℝ }
  deriving (Show)

sr_2stations :: AbsSlip -> AbsSlip -> CounterSlip
sr_2stations (AbsSlip spa1 spaU1 spe1 speU1)
             (AbsSlip spa2 spaU2 spe2 speU2)
        = CounterSlip (spa2 - spa1) (sqrt $ spaU1^2 + spaU2^2)

gpsStations_Average :: GPStations -> AbsSlip
gpsStations_Average stats = AbsSlip spa spaU spe speU
 where [spa,spe] = [ (/n) . sum $ extrSlip <$> absSlips
                   | extrSlip <- [slipParl, slipPerp] ]
       [spaU,speU] = [ sqrt . (/(n-1)) . sum $ ((^2) . (spa-) . extrSlip) <$> absSlips
                     | extrSlip <- [slipParl, slipPerp] ]
       n = fromIntegral $ length stats
       absSlips = snd <$> stats

