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
      tabular Nothing (replicate 4 CenterColumn) $ do
         "Station" &: math(mathit"Mag"<>mathrm"(SM)")
                   &: math(mathit"Mag"<>mathrm"(WA)")
                   &: math((mathit"Mag"<>mathrm"(SM)")/(mathit"Mag"<>mathrm"(WA)"))
         lnbk<>hline
         forM_ polandEQMeasure $ \(statName, m) -> do
            let fkt = amplificationFactors $ oscPeriodOfMaxAmpl m
            fromString statName
                   &: math (withUncertainty $ mag_SM fkt)
                   &: math (withUncertainty $ mag_WA fkt)
                   &: math (withUncertainty $ factor_SMbyWA fkt)
            lnbk
      newline
      "From this we find out the actual amplitude, as well as the amplitude that"
      " a Wood-Anderson seismometer would have displayed:"
      newline
      tabular Nothing (replicate 4 CenterColumn) $ do
         "Station" &: math"A"<>" in nm"
                   &: math("B"<>mathrm"(WA)")<>" in mm"
                   &: math(logBase 10 ("B"<>mathrm"(WA)"/physU"mm"))
         lnbk<>hline
         forM_ polandEQMeasure $ \(statName, m) -> do
            let fkt = amplificationFactors $ oscPeriodOfMaxAmpl m
            fromString statName
                   &: math (withUncertainty $ actualAmplitude m/nanometres)
                   &: math (withUncertainty $ woodAndersonAmplitude m/millimetres)
                   &: math (withUncertainty . logBase 10
                                 $ woodAndersonAmplitude m/millimetres)
            lnbk
   taskNo 3 "" $ do
      "Similarly to the amplification factor, we can interpolate the distance-dependent"
      " calibration amplitude ">>math("A"!:0)>>" (WRT a Wood-Anderson instrument)"
      " from a table due to Richter."
      " We also have given empirical formulas for calculating this value,"
      " for eastern North America due to Kim (1998)"
      " and Norway due to Alsaker et.al (1991)."
      mathDisplay' $ "A"!:(0<>mathrm"Kim")<>brak deltau
                 =: ( brak(deltau/physU"km")**(-1.45) )`cdot` (10**(-0.11))
      mathDisplay'' $ "A"!:(0<>mathrm"Alsaker")<>brak"R"
                 =: ( brak("R"/(17<>physU"km"))**(-0.776) )
                          `cdot` ( brak(10**0.000902)**(17 - "R"/physU"km") )
                                  / 100
      tabular Nothing (replicate 5 CenterColumn) $ do
         "Station" &: math"R"<>" in km"
                   &: math(("A"!:(0<>mathrm"Richter"))`cdot`(10**6))
                   &: math(("A"!:(0<>mathrm"Kim"))`cdot`(10**6))
                   &: math(("A"!:(0<>mathrm"Alsaker"))`cdot`(10**6))
         lnbk<>hline
         forM_ polandEQMeasure $ \(statName, m) -> do
            let r = hypoDist m
            fromString statName
                   &: math (withUncertainty $ r/kilometres)
                   &: math (withUncertainty $ distanceFactorRichter r*10^6)
                   &: math (withUncertainty $ distanceFactorKim r*10^6)
                   &: math (withUncertainty $ distanceFactorAlsaker r*10^6)
            lnbk
      newline
      "This factor influences the amplitude at a station of hypocentral or"
      " epicentral distance (which we equate, assuming a shallow earthquake)"
      " proportionally, so to get to the local magnitude we need to compute"
      mathDisplay'' $ "M"!|"L" =: logBase 10 ((("B"<>mathrm"(WA)")/physU"mm") / "A"!:0)
      tabular Nothing (replicate 5 CenterColumn) $ do
         "Station" &: math("B"<>mathrm"(WA)")<>" in mm"
                   &: math("M"!|"Richter")
                   &: math("M"!|"Kim")
                   &: math("M"!|"Alsaker")
         lnbk<>hline
         forM_ polandEQMeasure $ \(statName, m) -> do
            let r = hypoDist m
                bWA = woodAndersonAmplitude m/millimetres
            fromString statName
                   &: math (withUncertainty $ bWA)
                   &: math (withUncertainty . logBase 10 $ bWA / distanceFactorRichter r)
                   &: math (withUncertainty . logBase 10 $ bWA / distanceFactorKim r)
                   &: math (withUncertainty . logBase 10 $ bWA / distanceFactorAlsaker r)
            lnbk
   taskNo 4 "" $ do
      "The results with factors from Kim and Richter agree (just) within the uncertainty"
      " bounds. Note however that these uncertainties are coupled, so"
      " actually the discrepancy is significant."
      " Far more notable is how the distance for Norway expects much less attenuation"
      " and therefore estimates the seismic event at lower magnitude. Apparently"
      " geology on both American coasts causes the amplitude to drop faster until"
      " reaching such distances, than the Scandinavian does."
      lnbk
      "Also notable is how different the predictions from both stations are, as already"
      " evident from the seismograms in that MOX displays a ">>emph"higher">>" amplitude"
      " despite clearly being further away. This appears to be partly artifact of"
      " the method of measuring ">>emph"maximum amplitudes">>", which is not a very"
      " reliable way of classifying magnitude: since the processes are nontrivial and,"
      " involve substantial dispersion, the phase relations are almost statistical,"
      " which allows freak transients to be measured that far exceed the RMS amplitude."
      " But even that seems to be actually higher for the S-waves arriving at MOX than"
      " at CLL, so probably the source mechanisms actually sheds more energy in the"
      " direction of the former station."
   taskNo 8 "" $ do
      "bla"
     
      
      
      
      
      

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

-- | Special version of interpolation, since we need to carefully calculate
--   the quotient of the amplification factors.
amplificationFactors :: Uncertain Time -> AmplificationInfo
amplificationFactors (rT:±σT) = AmplificationInfo {
           amplif_T = rT:±σT
         , mag_SM = magSMinterp :± abs δmagSM * σT/δT
         , mag_WA = magWAinterp :± abs δmagWA * σT/δT
         , factor_SMbyWA = magSMinterp/magWAinterp
                            :± abs (δmagSM/magWAinterp - δmagWA*magSMinterp/magWAinterp^2)
                                   * σT/δT
         }
 where ηHi = (rT - expected (amplif_T infLo)) / δT
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


-- | Interpolate between the (ordered in the key, first element) list of (x,y) values.
interpolation :: [(ℝ,ℝ)] -> Uncertain ℝ -> Uncertain ℝ
interpolation l (x:±σx) = yInterp :± abs δy * σx/δx
 where ηHi = (x - xLo) / δx
       ηLo = 1 - ηHi
       δx = xHi - xLo
       δy = yHi - yLo
       
       yInterp = yLo*ηLo + yHi*ηHi
       
       (xHi,yHi):_ = dropWhile ((<x) . fst) l
       (xLo,yLo):_ = dropWhile ((>x) . fst) $ reverse l


distanceFactorKim :: Uncertain Distance -> Uncertain ℝ
distanceFactorKim r = (r/kilometres)**exactly(-1.45) * exactly (10**(-0.11))
            -- Actually works with Δ, not 𝑅. We assume shallow EQ.
            -- Form given in manual:
                -- - log₁₀ 𝐴₀ = 1.45*log₁₀ 𝑟 + 0.11   
                -- 1/𝐴₀ = 10^(1.45*log₁₀ 𝑟) * 10^0.11
                --      = 𝑟^1.45 * 10^0.11

distanceFactorAlsaker :: Uncertain Distance -> Uncertain ℝ
distanceFactorAlsaker r = (r/(17*kilometres))**exactly(-0.776)
                           * exactly(10**0.000902)**(17 - r/kilometres)
                           / 100
                -- - log₁₀ 𝐴₀ = 0.776 * log₁₀(𝑅/17) + 0.000902*(𝑅 - 17) + 2.0
                -- 1/𝐴₀ = 10^(0.776 * log₁₀(𝑅/17)) * 10^(0.000902*(𝑅 - 17)) * 100
                --      = (r/17)^0.776 * (10^0.000902)^(𝑅 - 17) * 100

distanceFactorRichter :: Uncertain Distance -> Uncertain ℝ
distanceFactorRichter = interpolation
                           [ (0*kilometres  , 10**(-1.4) )
                           , (10*kilometres , 10**(-1.5) )
                           , (20*kilometres , 10**(-1.7) )
                           , (30*kilometres , 10**(-2.1) )
                           , (40*kilometres , 10**(-2.4) )
                           , (50*kilometres , 10**(-2.6) )
                           , (60*kilometres , 10**(-2.8) )
                           , (70*kilometres , 10**(-2.8) )
                           , (80*kilometres , 10**(-2.9) )
                           , (90*kilometres , 10**(-3.0) )
                           , (100*kilometres, 10**(-3.0) )
                           , (120*kilometres, 10**(-3.1) )
                           , (140*kilometres, 10**(-3.2) )
                           , (160*kilometres, 10**(-3.3) )
                           , (180*kilometres, 10**(-3.4) )
                           , (200*kilometres, 10**(-3.5) )
                           , (220*kilometres, 10**(-3.65))
                           , (240*kilometres, 10**(-3.7) )
                           , (260*kilometres, 10**(-3.8) )
                           , (280*kilometres, 10**(-3.9) )
                           , (300*kilometres, 10**(-4.0) )
                           , (320*kilometres, 10**(-4.1) )
                           , (340*kilometres, 10**(-4.2) )
                           , (360*kilometres, 10**(-4.3) )
                           , (380*kilometres, 10**(-4.4) )
                           , (400*kilometres, 10**(-4.5) )
                           , (420*kilometres, 10**(-4.5) )
                           , (440*kilometres, 10**(-4.6) )
                           , (460*kilometres, 10**(-4.6) )
                           , (480*kilometres, 10**(-4.7) )
                           , (500*kilometres, 10**(-4.7) )
                           , (520*kilometres, 10**(-4.8) )
                           , (540*kilometres, 10**(-4.8) )
                           , (560*kilometres, 10**(-4.9) )
                           , (580*kilometres, 10**(-4.9) )
                           , (600*kilometres, 10**(-4.9) ) ]
