{-# LANGUAGE OverloadedStrings #-}


module LaTeX.GeoSeisExercise
               ( module Text.LaTeX
               , module Text.LaTeX.Packages.AMSMath
               , module Text.LaTeX.Packages.AMSFonts
               , module Text.LaTeX.Packages.Graphicx
               , ExerciseSheet, ExerciseSnippet
               , mkSolutionSheet
               , task, taskNo
               , (±)
               , realNum
               , physU, (*|:)
               , (!|)
               , printf
               , ℝ
               ) where

import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Commands
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSFonts
import Text.LaTeX.Packages.Graphicx

import Text.Printf
    
import qualified Data.Text.IO as Text

import Data.Char (isUpper)

type ExerciseSheet = IO ()
type ExerciseSnippet = LaTeXT IO

type ExerciseFunction = ExerciseSnippet () -> ExerciseSnippet ()
type ExerciseInfix = ExerciseSnippet () -> ExerciseSnippet () -> ExerciseSnippet ()

mkSolutionSheet :: ExerciseSnippet a -> ExerciseSheet
mkSolutionSheet snp = Text.putStr . render =<< execLaTeXT snp

taskNo :: Int -> LaTeX -> ExerciseSnippet a -> ExerciseSnippet a
taskNo n t e = raw ("\\exno["<>fromString(show n)<>"]{"<>render t<>"}") >> e

task :: LaTeX -> ExerciseSnippet a -> ExerciseSnippet a
task t e = section (fromLaTeX t) >> e

infixl 6 ±
(±) :: Double -> Double -> ExerciseSnippet ()
a ± σa = autoParens
         $ fromString (printf "%.*g" ndig a) +- fromString (printf "%.*g" ndig σa)
 where ndig = round $ logBase 10 (1/σa) + 1.5   :: Int -- ???

physU :: ExerciseFunction
physU = comm1 "physu"

infixl 6 *|:
(*|:) :: ExerciseInfix
v*|:u = v <> physU u

realNum :: Double -> ExerciseSnippet ()
realNum = fromString . reverse . dropWhile (=='0') . reverse . printf "%.10g"


(!|) :: ExerciseSnippet () -> String -> ExerciseSnippet ()
q!|c | all isUpper c  = q !: mathrm(""!:fromString c)
     | otherwise      = q !: mathrm (fromString c)


type ℝ = Double

