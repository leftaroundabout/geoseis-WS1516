{-# LANGUAGE OverloadedStrings, TypeFamilies, RankNTypes, UnicodeSyntax #-}


module LaTeX.GeoSeisExercise
               ( module Text.LaTeX
               , module Text.LaTeX.Packages.AMSMath
               , module Text.LaTeX.Packages.AMSFonts
               , module Text.LaTeX.Packages.Graphicx
               , module Data.VectorSpace, module Data.AffineSpace
               , module Graphics.Dynamic.Plot.R2
               , ExerciseSheet, ExerciseSnippet
               , mkSolutionSheet
               , task, taskNo
               , Uncertain((:±)), withUncertainty, (±), errorBarsPlot
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

import Graphics.Dynamic.Plot.R2
import Diagrams.Prelude ((^&), (#))
import qualified Diagrams.Prelude as Dia

import Data.AffineSpace
import Data.VectorSpace

import qualified Numeric.AD as AD
import qualified Numeric.GSL.Fitting as GSL

type ExerciseSheet = IO ()
type ExerciseSnippet = LaTeXT IO

type ExerciseFunction = ExerciseSnippet () -> ExerciseSnippet ()
type ExerciseInfix = ExerciseSnippet () -> ExerciseSnippet () -> ExerciseSnippet ()


data Uncertain v = (:±) {expected, uncertainty :: v}

instance RealFloat r => AdditiveGroup (Uncertain r) where
  zeroV = 0 :± 0
  negateV (a:±σ) = negate a :± σ
  (a:±σa) ^+^ (b:±σb) = (a+b) :± sqrt(σa^2 + σb^2)
instance RealFloat r => VectorSpace (Uncertain r) where
  type Scalar (Uncertain r) = r
  μ *^ (a:±σ) = μ*a :± abs μ*σ

errorBarsPlot :: [(Uncertain ℝ, Uncertain ℝ)] -> DynamicPlottable
errorBarsPlot ps = plot
   [ shapePlot $ Dia.place (Dia.rect (2*σx) (2*σy)) (x^&y)
                # Dia.fcA Dia.transparent
                         | (x:±σx, y:±σy) <- ps ]

withUncertainty :: Uncertain Double -> ExerciseSnippet ()
withUncertainty (a:±σ) = a ± σ

mkSolutionSheet :: ExerciseSnippet a -> ExerciseSheet
mkSolutionSheet snp = Text.putStr . render =<< execLaTeXT snp

taskNo :: Int -> LaTeX -> ExerciseSnippet a -> ExerciseSnippet a
taskNo n t e = raw ("\\exno["<>fromString(show n)<>"]{"<>render t<>"}") >> e

task :: LaTeX -> ExerciseSnippet a -> ExerciseSnippet a
task t e = section (fromLaTeX t) >> e

infixl 6 ±, :±
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


levMarFit :: (∀ x . Floating x => [x] -> x -> x)
             -> [(ℝ, Uncertain ℝ)] -> [ℝ] -> [Uncertain ℝ]
levMarFit f ps = map (uncurry(:±)) . fst
              . GSL.fitModelScaled 1e-4 1e-4 20 (ff,dff)
                      [(t, ([y], σy)) | (t, y:±σy)<-ps]
 where ff xs t = [(f :: [ℝ]->ℝ->ℝ) xs t]
       dff xs t = [tail $ (AD.grad (\(t':xs') -> f xs' t') :: [ℝ] -> [ℝ]) (t:xs)]
       
