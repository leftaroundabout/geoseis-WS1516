{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances, UndecidableInstances #-}

module Ex5.Solution where

import LaTeX.GeoSeisExercise

import Numeric.AD
import Numeric.GSL.Fitting

import Control.Monad


main :: ExerciseSheet
main = mkSolutionSheet $ do
   taskNo 1 "" $ do
     "Given is a ground model with an infinitely deep layer with wave speed ">>math("v"!:2)
     " below a layer of height ">>math"D">>" with wave speed ">>math("v"!:1)>>"."
     " In such a case there will always be a direct waves and reflected waves"
     " that both start from- and reach back to the surface. The direct wave simply gets"
     " to a distance of ">>math("v"!:1`cdot`"t")>>" by time ">>math"t">>", so the travel"
     " time to position ">>math"x">>" is"
     mathDisplay'' $ "t"!|"direct" =: "x"/"v"!:1
     "The reflected ray has to travel a path "
     mathDisplay $ "d"!|"refl" =: 2`cdot`sqrt("D"^:2 + brak("x"/2)^:2)
     " and also has always the speed ">>math("v"!:1)>>", so take a time of"
     mathDisplay'' $ "t"!|"refl" =: (2`cdot`sqrt("D"^:2 + brak("x"/2)^:2))/"v"!:1
     "If ">>math("v"!:2>:"v"!:1)>>", then there will also be a critically refracted wave"
     " to the surface. This always goes down, as well as up, in the critical angle with"
     mathDisplay'' $ sin(theta!|"c") =: "v"!:1/"v"!:2
     "The ray thus travels a horizontal distance component of"
     mathDisplay $ "x"!|"dcrit" =: "D"`cdot`tan(theta!|"c")
     "before hitting the layer boundary. The length of this path is"
     mathDisplay' $ "d"!|"dcrit"
           =: "D"`cdot`sqrt(1 + brak(tan(theta!|"c"))^:2)
     "and in between the points of critical refraction there is a distance of"
     mathDisplay $ "d"!|"bcrit"
           =: "x" - 2`cdot`"x"!|"dcrit"
     "at the speed of the lower layer, rendering the travel time as"
     mathDisplay'' $ "t"!|"crit"
           =: 2`cdot`"d"!|"dcrit"/"v"!:1 + "d"!|"bcrit"/"v"!:2
           =: 2`cdot`"D"`cdot`sqrt(1 + brak(tan(theta!|"c"))^:2)/"v"!:1
                 + ("x" - "D"`cdot`tan(theta!|"c"))/"v"!:2
   let travTimeDire _ v₁ _ x = x/v₁
       travTimeRefl d v₁ _ x = 2*sqrt(d^2 + (x/2)^2)/v₁
       travTimeCrit d v₁ v₂ x = let ϑc = asin (v₁/v₂)
                            in 2*d*sqrt(1 + tan ϑc^2)/v₁ + (x - d*tan ϑc^2)/v₂
   taskNo 2 "" $ do
--      liftIO $ plotWindow [ continFnPlot (travTimeDire 40 6 8) & legendName "direct"
--                          , continFnPlot (travTimeRefl 40 6 8) & legendName "reflected"
--                          , continFnPlot (travTimeCrit 40 6 8) & legendName "critical" ]
     includegraphics [IGScale 0.8] "Ex4/Traveltimes.png"
     "Since both the direct and critically reflected wave have an affine travel time,"
     " the takeover distance can easily be calculated."
     mathDisplay $ "t"!|"direct" =: "t"!|"crit"
     mathDisplay $ "x"!|"tkovr"/"v"!:1
              =: 2`cdot`"D"`cdot`sqrt(1 + brak(tan(theta!|"c"))^:2)/"v"!:1
                 + ("x"!|"tkovr" - "D"`cdot`tan(theta!|"c"))/"v"!:2
     mathDisplay $ "x"!|"tkovr"`cdot`"v"!:2
              =: 2`cdot`"D"`cdot`sqrt(1 + brak(tan(theta!|"c"))^:2)`cdot`"v"!:2
                 + ("x"!|"tkovr" - "D"`cdot`tan(theta!|"c"))`cdot`"v"!:1
     mathDisplay $ "x"!|"tkovr"`cdot`("v"!:2 - "v"!:1)
              =: 2`cdot`"D"`cdot`sqrt(1 + brak(tan(theta!|"c"))^:2)`cdot`"v"!:2
                 - "D"`cdot`tan(theta!|"c")`cdot`"v"!:1
     mathDisplay $ "x"!|"tkovr"
              =: ( 2`cdot`"D"`cdot`sqrt(1 + brak(tan(theta!|"c"))^:2)`cdot`"v"!:2
                  - "D"`cdot`tan(theta!|"c")`cdot`"v"!:1
                 ) /("v"!:2 - "v"!:1)
      
     return ()




         
                             


