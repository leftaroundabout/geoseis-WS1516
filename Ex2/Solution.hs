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
      figure Nothing . center $ do
         includegraphics [IGScale 0.5] "Ex2/GPSstats_measure-fault-dist.pdf"
         caption "Lines along which the distance from the fault is measured."
         label "GPSmeasureFaultDist"
      "From fig. ">>ref"GPSmeasureFaultDist">>", we measure the (roughly-) perpendicular"
      " distance of the GPS stations to the San Andreas Fault."
      lnbk
      displayGPSStations gpsStations
   let vOrig₀ = sumV (map gpsStat_slip_parl gpsStations)
                                    ^/ fromIntegral (length gpsStations)
       westmostStat = minimumBy (comparing gpsStat_distFromFault) gpsStations
       eastmostStat = maximumBy (comparing gpsStat_distFromFault) gpsStations
       vSlip₀ = gpsStat_slip_parl westmostStat ^-^ gpsStat_slip_parl eastmostStat
       d₀ = 15:±5 :: Uncertain Distance
       elasticFaultSlip [vOrig, vSlip, d] x = vOrig - vSlip * (atan(x/d)/pi)
   taskNo 3 "" $ do
      "We'd like to fit a model"
      mathDisplay $ "v"!|"x" =: elasticFaultSlip ["v"!|"orig", "v"!|"slip", "D"] "x"
      "to the data. For a first estimate, use"
      items [
         do "The average parallel velocity of all stations as ">>math("v"!|"orig")
            " (uncertainty from Gauss propagation, not standard deviation)."
            mathDisplay $
               "v"!|"orig" =: 1/"n" * tsum<>"v"!:"||"
                           =: withUncertainty vOrig₀ *|: "mm"/"yr"
       , do "The velocity-difference between the furthest opposed stations as "
            math("v"!|"slip")>>"."
            mathDisplay $
               "v"!|"slip" =: "v"!|gpsStat_Name westmostStat
                              - "v"!|gpsStat_Name eastmostStat
                           =: withUncertainty vSlip₀ *|: "mm"/"yr"
       , do "The a-priori value ">>math("D"=:withUncertainty d₀*|:"km")>>"."
       ]
   taskNo 6 "" $ do
      "As seen in fig. "<>ref"GPSparVeloPlot"<>", the a-priori model does not describe the"
      " GPS data within the uncertainty bounds, though the qualitative shape matches."
      " This hints that the model itself might be valid, but the parameters not."
   taskNo 7 "" $ do
      "To obtain more accurate values of the model parameters, we use"
      " the Levenberg-Marquard algorithm"
      footnote $ "Invoked from the GNU Scientific Library implementation. "<>
         "http://hackage.haskell.org/package/hmatrix-gsl-0.17.0.0/docs/Numeric-GSL-Fitting.html"
      " to get a least-squares fit. For the sake of simplicity,"
      " ignore the uncertainties of the distance measurements, since the slip-velocity"
      " has a larger relative uncertainty."
      let [vOrig,vSlip,d] = levMarFit elasticFaultSlip
                                  [ (expected dist,parls)
                                      | GPSStation _ dist parls _ <- gpsStations]
                                  [vOrig₀, vSlip₀, d₀]
      "Result:"
      mathDisplay $ "v"!|"orig" =: withUncertainty vOrig *|: "mm"/"yr"
      mathDisplay $ "v"!|"slip" =: withUncertainty vSlip *|: "mm"/"yr"
      mathDisplay $ "D" =: withUncertainty d*|:"km"
      const (return ())
           $ plotWindow [ errorBarsPlot [(d,v) | GPSStation _ d v _ <- gpsStations]
                          & legendName "GPS"
                        , (continFnPlot . elasticFaultSlip $ expected<$>[vOrig₀,vSlip₀,d₀])
                          & legendName "A priori"
                        , (continFnPlot . elasticFaultSlip $ expected<$>[vOrig, vSlip, d ])
                          & legendName "Lev-Marq" ]
      figure Nothing . center $ do
         includegraphics [IGScale 0.7] "Ex2/slipfit.png"
         caption "Plot of the fault-parallel velocity, depending on the distance \
                 \from the fault. Comparison of the two parameter models."
         label "GPSparVeloPlot"
   taskNo 8 "" $ do
      "Fig. "<>ref"GPSparVeloPlot"<>" shows that the least-square fit can indeed bring"
      " the elastic model to within the uncertainty of the GPS data (for some stations"
      " it still lies without, but overall there is no significant discrepancy)."
      " So the model of an elastic seismogenic zone around the locked fault appears to"
      " work for the San Andreas Fault. The method of determining the parameters in"
      " task 3 is not sufficient, because especially on the north-eastern side all of"
      " the stations actuall lie on the deformed zone, hence can't indicate the full"
      " slip movement."
                                  
      
      
data GPSStation = GPSStation {
        gpsStat_Name :: String
      , gpsStat_distFromFault :: Uncertain Distance
      , gpsStat_slip_parl, gpsStat_slip_perp :: Uncertain Velo
      }

type Distance = ℝ -- in km
type Velo = ℝ -- in mm/yr

px2d :: Px -> Uncertain Distance
px2d px = (px :± 2) ^* (100 / km100)
 where km100 = 428.30 -- uncertainty small, compared to distances



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
      "Site" &: "Distance from fault" &: "Fault "<>math"||" &: ("Fault "<>math bot)
      lnbk<>hline
      "Name" &: "Distance [km]" &: "Slip rate [mm/yr]" &: "Slip rate [mm/yr]"
      lnbk<>hline
      forM_ stats $ \(GPSStation nm pdist spa spe) -> do
          fromString nm &: math (withUncertainty pdist)
                        &: math (withUncertainty spa)
                        &: math (withUncertainty spe)
          lnbk
         
                             


