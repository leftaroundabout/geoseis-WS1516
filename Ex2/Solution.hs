{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances, UndecidableInstances #-}

module Ex2.Solution where

import LaTeX.GeoSeisExercise
import Ex1.Solution (AbsSlip(..))

import Numeric.AD
import Numeric.GSL.Fitting

import Control.Monad


main :: ExerciseSheet
main = mkSolutionSheet $ do
   taskNo 1 "" $ do
      figure (Just Bottom) . center $ do
         includegraphics [IGScale 0.5] "Ex2/GPSstats_measure-fault-dist.pdf"
         caption "Lines along which the distance from the fault is measured."
         label "GPSmeasureFaultDist"
      "From fig ">>ref"GPSmeasureFaultDist">>", we measure the (roughly-) perpendicular"
      " distance of the GPS stations to the San Andreas Fault."
      lnbk
      displayGPSStations gpsStations
   taskNo 7 "" $ do
      "We'd like to fit a model"
      mathDisplay $ "v"!|"x" =: elasticFaultSlip ["v"!|"orig", "v"!|"slip", "D"] "x"
      "to the data. Use the Levenberg-Marquard algorithm. For the sake of simplicity,"
      " ignore the uncertainties of the distance measurements, since the slip-velocity"
      " has a larger relative uncertainty."
--      let parlslips = [(expected dist,parls) | GPSStation _ dist parls _ <- gpsStations]
 --         ([vOrig,vSlip,d],_) = levMarFit elasticFaultSlip [
--       lift $ plotWindow [errorBarsPlot []
--       [vWest, vEast] <- forM [(gpsStationsWest, "west"), (gpsStationsEast, "east")]
--                         $ \(sta, dir) -> do
--           let slip@(AbsSlip spa spaU _ _) = gpsStations_Average sta
--           mathDisplay $ "v"!|dir =: (spa ± spaU) <> physU(tfrac"mm""yr")
--           return slip
      
      
data GPSStation = GPSStation {
        gpsStat_Name :: String
      , gpsStat_distFromFault :: Uncertain Distance
      , gpsStat_slip_parl, gpsStat_slip_perp :: Uncertain Velo
      }

type Distance = ℝ -- in km
type Velo = ℝ -- in mm/yr
type Px = Double

px2d :: Px -> Uncertain Distance
px2d px = (px :± 2) ^* (100 / km100)
 where km100 = 428.30 -- uncertainty small, compared to distances


elasticFaultSlip [vOrig, vSlip, d] x = vOrig + vSlip * (atan(x/d)/pi)


gpsStations :: [GPSStation]
gpsStations = [ GPSStation "A504" (px2d$  -91.67) (37.5:±0.6) (3.0:±0.4)
              , GPSStation "A505" (px2d$ -286.34) (42.9:±1.4) (4.1:±0.5)
              , GPSStation "LNDA" (px2d$ -239.08) (42.5:±0.5) (4.4:±0.5)
              , GPSStation "SAL2" (px2d$  -66.21) (36.0:±0.6) (1.6:±0.4)
              , GPSStation "POSO" (px2d$  -15.82) (32.0:±1.2) (0.9:±0.3)
              , GPSStation "CAUV" (px2d$   20.11) (25.8:±0.5) (2.9:±0.5)
              , GPSStation "GOUD" (px2d$   59.49) (23.4:±0.3) (1.9:±0.1)
              , GPSStation "LASY" (px2d$    9.85) (29.3:±0.4) (2.5:±0.3)
              , GPSStation "NAPO" (px2d$   27.78) (25.4:±0.8) (3.9:±0.7)
              , GPSStation "P807" (px2d$   91.31) (19.8:±0.5) (3.4:±0.5)
              , GPSStation "TWR2" (px2d$    6.11) (29.7:±0.8) (3.5:±0.4) ]




displayGPSStations :: [GPSStation] -> ExerciseSnippet ()
displayGPSStations stats =
   tabular Nothing (replicate 4 CenterColumn) $ do
      "Site" & "Distance from fault" & "Fault "<>math"||" & ("Fault "<>math bot)
      lnbk<>hline
      "Name" & "Distance [km]" & "Slip rate [mm/yr]" & "Slip rate [mm/yr]"
      lnbk<>hline
      forM_ stats $ \(GPSStation nm pdist spa spe) -> do
          fromString nm & math (withUncertainty pdist)
                        & math (withUncertainty spa)
                        & math (withUncertainty spe)
          lnbk
         
                             



data CounterSlip = CounterSlip { counterSlip, csUncert :: ℝ }
  deriving (Show)

sr_2stations :: AbsSlip -> AbsSlip -> CounterSlip
sr_2stations (AbsSlip spa1 spaU1 spe1 speU1)
             (AbsSlip spa2 spaU2 spe2 speU2)
        = CounterSlip (spa2 - spa1) (sqrt $ spaU1^2 + spaU2^2)


