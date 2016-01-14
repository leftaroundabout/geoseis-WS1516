{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances, UndecidableInstances #-}

module Ex7.Solution where

import LaTeX.GeoSeisExercise

import Numeric.AD
import Numeric.GSL.Fitting

import Control.Monad


main :: ExerciseSheet
main = mkSolutionSheet $ do
   taskNo 1 "" $ do
      "We measure arrivals from a seismic event in Poland"
      " (zero of time scale is arbitrary), and can calculate approximate"
      " hypocentral distances using the formula"
      mathDisplay $ brak(tau!:(mathrm$"S"!:"g") - tau!:(mathrm$"P"!:"g")) `cdot` 8
                     =: "R"
      figure Nothing . center $ do
         includegraphics [IGScale 0.5] "Ex7/arrivals.pdf"
         caption "Arrival times of the waves from the seismic event in Poland."
         label "arrivals"
      newline
      tabular Nothing (replicate 5 CenterColumn) $ do
         "Station" &: math(tau!:(mathrm$"P"!:"n"))<>" in s"
                   &: math(tau!:(mathrm$"P"!:"g"))<>" in s"
                   &: math(tau!:(mathrm$"S"!:"g"))<>" in s"
                   &: math("R")<>" in km"
         lnbk<>hline
         forM_ polandEQMeasure $ \(statName, m) -> do
            fromString statName &: math (withUncertainty $ pnArrive m/seconds)
                                &: math (withUncertainty $ pgArrive m/seconds)
                                &: math (withUncertainty $ sgArrive m/seconds)
                                &: math (withUncertainty $ hypoDist m/kilometres)
            lnbk
   taskNo 2 "" $ do
      "We can also measure the maximum amplitudes and their respective oscillation periods:"
      figure Nothing . center $ do
         includegraphics [IGScale 1.4] "Ex7/amplitudes.pdf"
         caption "Maximum amplitudes of the waves from the seismic event in Poland."
         label "amplitudes"
      newline
      tabular Nothing (replicate 3 CenterColumn) $ do
         "Station" &: math"B"<>" in cm"
                   &: math"T"<>" in s"
         lnbk<>hline
         forM_ polandEQMeasure $ \(statName, m) -> do
            fromString statName &: math (withUncertainty $ measuredAmplitude m/centimetres)
                                &: math (withUncertainty $ oscPeriodOfMaxAmpl m/seconds)
            lnbk
      newline
      "From a given table, we know the amplification factors around those frequencies,"
      " both for the instruments used at these stations (SM) and the Wood-Anderson"
      " seismograph which defines the Richter scale:"
      newline
      tabular Nothing (replicate 3 CenterColumn) $ do
         math"T"<>" in s" &: math(mathit"Mag"<>mathrm"(SM)")
                          &: math(mathit"Mag"<>mathrm"(WA)")
         lnbk<>hline
         forM_ amplificationFactorsDb $ \fts -> do
            math(showInTeX $ amplif_T fts) &: math (withUncertainty $ mag_SM fts)
                                           &: math (withUncertainty $ mag_WA fts)
            lnbk
      newline
      "Use linear interpolation (uncertainties propagated according to difference"
      " quotient between considered period-times and their corresponding amplification)"
      " to find actual amplification factors for the measured frequencies."
      newline
      tabular Nothing (replicate 3 CenterColumn) $ do
         "Station" &: math(mathit"Mag"<>mathrm"(SM)")
                   &: math((mathit"Mag"<>mathrm"(SM)")/(mathit"Mag"<>mathrm"(WA)"))
         lnbk<>hline
         forM_ polandEQMeasure $ \(statName, m) -> do
            let fkt = amplificationFactors $ oscPeriodOfMaxAmpl m
            fromString statName
                   &: math (withUncertainty $ mag_SM fkt)
                   &: math (withUncertainty $ factor_SMbyWA fkt)
            lnbk
      newline
      "From this we find out the actual amplitude, as well as the amplitude that"
      " a Wood-Anderson seismometer would have displayed:"
      newline
      tabular Nothing (replicate 4 CenterColumn) $ do
         "Station" &: math"A"<>" in nm"
                   &: math"B(WA)"<>" in cm"
                   &: math(logBase 10 ("B(WA)"/physU"mm"))
         lnbk<>hline
         forM_ polandEQMeasure $ \(statName, m) -> do
            let fkt = amplificationFactors $ oscPeriodOfMaxAmpl m
            fromString statName
                   &: math (withUncertainty $ actualAmplitude m/nanometres)
                   &: math (withUncertainty $ woodAndersonAmplitude m/centimetres)
                   &: math (withUncertainty . logBase 10
                                 $ woodAndersonAmplitude m/millimetres)
            lnbk
      
      
      
      

type Time = ℝ
seconds = 1
minutes = 60*seconds

type Distance = ℝ
type Displacement = Distance
metres = 1
centimetres = metres/100
millimetres = metres/1000
mikrometres = millimetres/1000
nanometres = mikrometres/1000
kilometres = 1000*metres


data EqMeasure = EqMeasure {
      pnArrive, pgArrive, sgArrive :: Uncertain Time
    , hypoDist :: Uncertain Distance
    , measuredAmplitude
      , actualAmplitude
      , woodAndersonAmplitude :: Uncertain Displacement
    , oscPeriodOfMaxAmpl :: Uncertain Time
    }


polandEQMeasure :: [(String, EqMeasure)]
polandEQMeasure = second completeMeasure
        <$> [ ( "CLL"
              , EqMeasure { pnArrive = px2Time $ fromRange (236.9, 244.5)
                          , pgArrive = px2Time $ fromRange (259.1, 270.7)
                          , sgArrive = px2Time $ fromRange (404.8, 416.7)
                          , measuredAmplitude
                            = ( px2Displacement (fromRange (441.4, 442.7))
                              - px2Displacement (fromRange (315.7, 319.3))
                              ) / 2
                          , actualAmplitude = undefined -- obtained with `completeMeasure`
                          , woodAndersonAmplitude = undefined
                          , oscPeriodOfMaxAmpl
                            =  px2Time (fromRange (463.9, 466.1))
                             - px2Time (fromRange (459.0, 461.2))
                          , hypoDist = undefined -- obtained with `completeMeasure`
                          } )
            , ( "MOX"
              , EqMeasure { pnArrive = px2Time $ fromRange (327.3, 333.4)
                          , pgArrive = px2Time $ fromRange (372.8, 379.3)
                          , sgArrive = px2Time $ fromRange (603.6, 616.7)
                          , measuredAmplitude
                            = ( px2Displacement (fromRange (331.4, 335.0))
                              - px2Displacement (fromRange (108.6, 110.7))
                              ) / 2
                          , actualAmplitude = undefined
                          , woodAndersonAmplitude = undefined
                          , oscPeriodOfMaxAmpl
                            =  px2Time (fromRange (678.6, 680.9))
                             - px2Time (fromRange (671.7, 674.0))
                          , hypoDist = undefined } ) ]
 where Iso px2Time _ = pixelVsQuantity ((367.14*px, 721.43*px), 1*minutes) (40*px)
       Iso px2Displacement _ = pixelVsQuantity 
                                  ((367.14*px, 721.43*px), 5.7*centimetres) (375.8*px)
       completeMeasure m = m { hypoDist = (sgArrive m - pgArrive m) / seconds
                                          * 8 * kilometres
                             , actualAmplitude = measuredAmplitude m / mag
                             , woodAndersonAmplitude = measuredAmplitude m / fact
                             }
        where (mag,fact) = (mag_SM&&&factor_SMbyWA)
                           . amplificationFactors $ oscPeriodOfMaxAmpl m
                             


data AmplificationInfo = AmplificationInfo { amplif_T :: Uncertain Time
                                           , mag_SM, mag_WA :: Uncertain ℝ
                                           , factor_SMbyWA :: Uncertain ℝ
                                           }
amplificationFactors :: Uncertain Time -> AmplificationInfo
amplificationFactors (rT:±σT) = AmplificationInfo {
           amplif_T = rT:±σT
         , mag_SM = magSMinterp :± abs δmagSM * σT/δT
         , mag_WA = magWAinterp :± abs δmagWA * σT/δT
         , factor_SMbyWA = magSMinterp/magWAinterp
                            :± abs (δmagSM/magWAinterp - δmagWA*magSMinterp/magWAinterp^2)
                                   * σT/δT
         }
 where ηHi = (rT - expected (amplif_T infHi)) / δT
       ηLo = 1 - ηHi
       δT = expected (amplif_T infHi) - expected (amplif_T infLo)
       
       magSMinterp = expected(mag_SM infLo)*ηLo + expected(mag_SM infHi)*ηHi
       δmagSM = expected(mag_SM infHi) - expected(mag_SM infLo)
       magWAinterp = expected(mag_WA infLo)*ηLo + expected(mag_WA infHi)*ηHi
       δmagWA = expected(mag_WA infHi) - expected(mag_WA infLo)
       
       infHi:_ = dropWhile ((<rT) . expected . amplif_T) amplificationFactorsDb
       infLo:_ = dropWhile ((>rT) . expected . amplif_T) $ reverse amplificationFactorsDb

amplificationFactorsDb :: [AmplificationInfo]
amplificationFactorsDb = [ AmplificationInfo (0.8*seconds) 201000 1600 undefined
                         , AmplificationInfo (0.9*seconds) 201000 1400 undefined
                         , AmplificationInfo (1.0*seconds) 200000 1200 undefined
                         , AmplificationInfo (1.1*seconds) 190000 1100 undefined
                         , AmplificationInfo (1.2*seconds) 180000  950 undefined ]


