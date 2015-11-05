{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances, UndecidableInstances #-}


import LaTeX.GeoSeisExercise
import Control.Monad


main :: ExerciseSheet
main = mkSolutionSheet $ do
   taskNo 1 "" $ do
      "Given is the following data from GPS stations near the San Andreas Fault: west of the fault (on the Pacific Plate),"
      lnbk
      displayGPSStations gpsStationsWest
      lnbk
      "and east of it (North American Plate):"
      lnbk
      displayGPSStations gpsStationsEast
      lnbk



displayGPSStations :: forall m . (Monad m) => GPStations -> LaTeXT m ()
displayGPSStations stats =
   tabular Nothing (replicate 3 CenterColumn) $ do
      "Site" & "Fault ||" & ("Fault "<>math bot)
      lnbk<>hline
      "Name" & "Rate" & "Rate"
      lnbk<>hline
      forM_ stats $ \(nm, AbsSlip spa spaU spe speU) -> do
          fromString nm & math(fromString(show spa) +- fromString(show spaU))
                        & math(fromString(show spe) +- fromString(show speU))
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
       [spaU,speU] = [ sqrt . (/n) . sum $ ((^2) . extrUncrt) <$> absSlips
                     | extrUncrt <- [spaUncert, speUncert] ]
       n = fromIntegral $ length stats
       absSlips = snd <$> stats

type ℝ = Double

