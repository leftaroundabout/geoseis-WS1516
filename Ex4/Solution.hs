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
      "The pixel coordinates of the given seismic stations are">>newline
      forM_ seisStations $ \(SeisStation name pos _ _) -> do
         fromString $ show (name, coords2px pos)
         newline
   taskNo 2 "" $ do
      "These stations registered the earthquake at times">>newline
      forM_ seisStations $ \(SeisStation name _ tp ts) -> do
         fromString name >> ": "
         math ("t"!|"p" =: "5:19:">>withUncertainty tp) >> " h , "
         math ("t"!|"s" =: "5:19:">>withUncertainty ts) >> " h"
         newline
   taskNo 3 "" $ do
      "The wave travel times obey"
      mathDisplay $ "t"!|"s" - "t"!:0 =: "R"/"v"!|"s"
      mathDisplay $ "t"!|"p" - "t"!:0 =: "R"/"v"!|"p"
      "The time ">>math("t"!:0)>>" of the earthquake itself is not known, but the"
      " difference between P- and S-wave arrival is ">>math tau>>"."
      " Consider the difference of the above equations:"
      mathDisplay $ "R"/"v"!|"s" - "R"/"v"!|"p"
                    =: "t"!|"s" - "t"!:0 - brak("t"!|"p" - "t"!:0)
                    =: "t"!|"s" - "t"!|"p"
                    =: tau
      mathDisplay $ "v"!|"p" * "v"!|"s" * tau
                    =: "R"*"v"!|"p" - "R"*"v"!|"s"
                    =: "R"*brak("v"!|"p" - "v"!|"s")
      mathDisplay $ "R" =: tau * (("v"!|"p" * "v"!|"s")/("v"!|"p" - "v"!|"s"))
      "In an ideal elastic medium,"
      mathDisplay $ "v"!|"s" =: "v"!|"p" / sqrt 3
      "so"
      mathDisplay $ "R" =: tau * (("v"!|"p"^:2)/("v"!|"p"`cdot`sqrt 3 - "v"!|"p"))
                        =: tau * ("v"!|"p"/(sqrt 3 - 1))
   taskNo 4 "" $ do
      "To infer propagated errors through a formula, consider the usuall Gaussian"
      " error propagation for statistically independent deviations."
   taskNo 5 "" $ do
      let vp = 6000 :± 500
      "Assume now ">>math("v"!|"p" =: withUncertainty vp *|: "m"/"s")>>"."
      " This gives rise to the following distances of the stations from the epicenter:"
      newline
      forM_ seisStations $ \(SeisStation name _ tp ts) -> do
         fromString name >> ": "
         let r = vp * (ts - tp) / (sqrt 3 - 1)
         math $ "R" =: withUncertainty r *|: "m"
         ", or ">>math (withUncertainty (distToPx r) *|: "Px")>>"."
         newline
   taskNo 6 "" $ do
      "Graphically intersecting the circles of these distances from the stations gives"
      " an epicenter location of ("
      let (coX, coY) = px2coords (461.27:±15, 98.3:±13)
      math (showLatitude coY) >> ", " >> math (showLongitude coX) >> ")."
      
      
data SeisStation = SeisStation {
        seisStat_Name :: String
      , seisStat_location :: (Uncertain Latitude, Uncertain Longitude)
      , seisStat_tp, seisStat_ts :: Uncertain Time
      }

type Distance = ℝ -- in m

type Speed = ℝ -- in m/s

type Time = ℝ -- in seconds after 5:19 on the day of the earthquake

type Px = Double

coords2px :: (Uncertain Latitude, Uncertain Longitude) -> (Uncertain Px, Uncertain Px)
px2coords :: (Uncertain Px, Uncertain Px) -> (Uncertain Latitude, Uncertain Longitude)
(coords2px, px2coords)
         = (\(cy,cx) -> ( px + (cx-ex)*(qx-px)/(fx-ex), py + (cy-ey)*(qy-py)/(fy-ey) )
           ,\(tx,ty) -> ( ey + (ty-py)*(fy-ey)/(qy-py), ex + (tx-px)*(fx-ex)/(qx-px) ) )
 where [((px,py), (ey,ex)), ((qx,qy),(fy,fx))]
         = [ ((185.64, 138.09), (exactly 50.5°N, 6°E))
           , ((585.94, 453.91), (exactly 51.5°N, 8°E)) ]

distToPx :: Uncertain Distance -> Uncertain Px
distToPx = (*146.92) . (/exactly 50e+3)

seisStations :: [SeisStation]
seisStations = [ SeisStation "BGG" (50.206°N, 7.337°E) (7.55:±0.1)  (10.15:±0.1)
               , SeisStation "BNS" (50.964°N, 7.176°E) (14.75:±0.1) (21.95:±0.2)
               , SeisStation "KLL" (50.647°N, 6.312°E) (17.1:±0.25) (27.5:±1.1)
               , SeisStation "ROD" (51.145°N, 6.181°E) (23.7:±0.23) (38.2:±0.5)
               , SeisStation "STB" (50.594°N, 6.840°E) (11.45:±0.2) (16.57:±0.3) ]




         
                             


