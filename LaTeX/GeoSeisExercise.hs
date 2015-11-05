{-# LANGUAGE OverloadedStrings #-}


module LaTeX.GeoSeisExercise
               ( module Text.LaTeX
               , module Text.LaTeX.Packages.AMSMath
               , module Text.LaTeX.Packages.AMSFonts
               , module Text.LaTeX.Packages.Graphicx
               , ExerciseSheet, ExerciseSnippet
               , mkSolutionSheet
               , task, taskNo
               ) where

import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Commands
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSFonts
import Text.LaTeX.Packages.Graphicx
    
import qualified Data.Text.IO as Text

type ExerciseSheet = IO ()
type ExerciseSnippet = LaTeXT IO

mkSolutionSheet :: ExerciseSnippet a -> ExerciseSheet
mkSolutionSheet snp = Text.putStr . render =<< execLaTeXT snp

taskNo :: Int -> LaTeX -> ExerciseSnippet a -> ExerciseSnippet a
taskNo n t e = raw ("\\exno["<>fromString(show n)<>"]{"<>render t<>"}") >> e

task :: LaTeX -> ExerciseSnippet a -> ExerciseSnippet a
task t e = section (fromLaTeX t) >> e


