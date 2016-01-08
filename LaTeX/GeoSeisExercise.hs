{-# LANGUAGE OverloadedStrings, TypeFamilies, RankNTypes, UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}


module LaTeX.GeoSeisExercise
               ( module Text.LaTeX
               , module Text.LaTeX.Packages.AMSMath
               , mathDisplay', mathDisplay''
               , module Text.LaTeX.Packages.AMSFonts
               , module Text.LaTeX.Packages.Graphicx
               , module Data.Ord, module Data.List
               , (&)
               , module Data.VectorSpace, module Data.AffineSpace
               , module Graphics.Dynamic.Plot.R2
               , module Control.Arrow, module Control.Monad
               , ExerciseSheet, ExerciseSnippet
               , mkSolutionSheet
               , task, taskNo
               , items
               , Uncertain(..), withUncertainty, exactly, (±), errorBarsPlot
               , realNum
               , physU, (*|:)
               , (!|)
               , (&:)
               , (⎛), brak
               , printf, showInTeX
               , ℝ
               , levMarFit
               , Angle, Azimuth, Dip, Latitude, Longitude
               , (°), LatitudeDirection(..), LongitudeDirection(..), AngleDirection(..)
                    , showAngle, showLatitude, showLongitude
               ) where

import Text.LaTeX hiding ((&))
import qualified Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Commands hiding ((&))
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSFonts
import Text.LaTeX.Packages.Graphicx

import Text.Printf
    
import qualified Data.Text.IO as Text

import Data.Char (isUpper)
import Data.Ord (comparing)
import Data.Function ((&))
import Data.List (minimumBy, maximumBy)

import Graphics.Dynamic.Plot.R2
import Diagrams.Prelude ((^&), (#))
import qualified Diagrams.Prelude as Dia

import Data.AffineSpace
import Data.VectorSpace
import Data.Ratio

import qualified Numeric.AD as AD
import qualified Numeric.GSL.Fitting as GSL

import Control.Monad
import Control.Arrow

import Numeric (readFloat)

type ExerciseSheet = IO ()
type ExerciseSnippet = LaTeXT IO

type ExerciseFunction = ExerciseSnippet () -> ExerciseSnippet ()
type ExerciseInfix = ExerciseSnippet () -> ExerciseSnippet () -> ExerciseSnippet ()


data Uncertain v = (:±) {expected, uncertainty :: v}

exactly :: Num v => v -> Uncertain v
exactly = (:±0)

instance RealFloat r => AdditiveGroup (Uncertain r) where
  zeroV = exactly 0
  negateV (a:±σ) = negate a :± σ
  (a:±σa) ^+^ (b:±σb) = (a+b) :± sqrt(σa^2 + σb^2)
instance RealFloat r => VectorSpace (Uncertain r) where
  type Scalar (Uncertain r) = r
  μ *^ (a:±σ) = μ*a :± abs μ*σ
instance RealFloat r => Eq (Uncertain r) where
  a:±σa == b:±σb = a+σa > b-σb && a-σa < b+σb
instance RealFloat r => Ord (Uncertain r) where
  compare a b | a==b                     = EQ
              | expected a < expected b  = LT
              | otherwise                = GT

instance RealFloat r => Num (Uncertain r) where
  fromInteger = (:±0) . fromInteger
  negate = negateV
  (+) = (^+^)
  (x:±σx)*(y:±σy) = x*y :± sqrt((x*σy)^2 + (σx*y)^2)
  abs (x:±σx) = abs x:±σx
  signum (x:±_) = signum x:±0
instance RealFloat r => Fractional (Uncertain r) where
  fromRational fr = (fromIntegral(numerator fr):±1) / fromIntegral(denominator fr)
  recip (x:±σx) = recip x :± σx/x^2
instance RealFloat r => Floating (Uncertain r) where
  pi = fromRational 3.141592653589793
  sqrt (x:±σx) = sqrtx :± σx/(2*sqrtx)
   where sqrtx = sqrt x

instance (RealFloat r) => Read (Uncertain r) where
  readsPrec p s | '.'`elem`s  = first fromRational <$> readFloat s
                | otherwise   = first fromInteger <$> readsPrec p s
  

deciRound :: RealFloat r => Uncertain r -> r
deciRound (x:±σx) = (*e) . fromIntegral . round $ x / e
 where e = 10^^(floor $ logBase 10 σx)

instance (Show r, RealFloat r) => Show (Uncertain r) where
  show (x:±0) | xi<-round x, x==fromInteger xi = show xi
              | otherwise   = show x
  show xu = reverse . dropWhile(=='e') . cleanup . ('e':) . reverse . show $ deciRound xu
   where cleanup ('e':_:s@('9':'9':'9':_)) = kill9s s
         cleanup ('e':_:s@('0':'0':'0':_)) = kill0s s
         cleanup (c:s) = c : cleanup s
         cleanup [] = []
         kill9s ('9':s) = kill9s s
         kill9s (d:s) | d`elem`['0'..'8'] = succ d : s
         kill9s ('.':s) = let sk = kill9s s
                          in replicate (length s - length sk) '0' ++ sk
         kill9s s = '1':s
         kill0s = dropWhile (=='0')


showInTeX :: (Show a, LaTeXC l) => a -> l
showInTeX = fromString . show

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
         $ fromString (printf "%.*f" ndig a) +- fromString (printf "%.*f" ndig σa)
 where ndig = round $ logBase 10 (1/σa) + 1.5   :: Int -- ???

physU :: ExerciseFunction
physU = comm1 "physu"

infixl 6 *|:
(*|:) :: ExerciseInfix
v*|:u = v <> physU u

realNum :: Double -> ExerciseSnippet ()
realNum = fromString . reverse . dropWhile (=='0') . reverse . printf "%.10g"

items :: [ExerciseSnippet ()] -> ExerciseSnippet ()
items itmz = itemize . forM_ itmz $ \i -> item Nothing >> i

(!|) :: ExerciseSnippet () -> String -> ExerciseSnippet ()
q!|c | all isUpper c  = q !: mathrm(""!:fromString c)
     | otherwise      = q !: mathrm (fromString c)


infixl 1 &:
(&:) :: LaTeXC l => l -> l -> l
(&:) = (Text.LaTeX.&)

type ℝ = Double


levMarFit :: (∀ x . Floating x => [x] -> x -> x)
             -> [(ℝ, Uncertain ℝ)] -> [Uncertain ℝ] -> [Uncertain ℝ]
levMarFit f ps = map (uncurry(:±)) . fst
              . GSL.fitModelScaled 1e-4 1e-4 20 (ff,dff)
                      [(t, ([y], σy)) | (t, y:±σy)<-ps]
              . map expected
 where ff xs t = [(f :: [ℝ]->ℝ->ℝ) xs t]
       dff xs t = [tail $ (AD.grad (\(t':xs') -> f xs' t') :: [ℝ] -> [ℝ]) (t:xs)]


data LatitudeDirection = N | S
data LongitudeDirection = E | W
data AngleDirection = R | L

class CoordDirection d where
  (°) :: Floating r => r -> d -> r
infixl 8 °
instance CoordDirection LatitudeDirection where
  θ°N = θ*pi/180; θ°S = - θ*pi/180
instance CoordDirection LongitudeDirection where
  λ°E = λ*pi/180; λ°W = - λ*pi/180
instance CoordDirection AngleDirection where
  α°R = α*pi/180; α°L = - α*pi/180
        
type Angle = ℝ
type Latitude = Angle
type Longitude = Angle
type Azimuth = Angle
type Dip = Angle

showAngle :: Uncertain Angle -> ExerciseSnippet ()
showAngle α = withUncertainty (α*180/pi) >> ""^:(circ"""")
  
showLatitude :: Uncertain Latitude -> ExerciseSnippet ()
showLatitude θ | θ>0  = showAngle θ >> physU "N"
showLongitude :: Uncertain Longitude -> ExerciseSnippet ()
showLongitude λ | λ>0  = showAngle λ >> physU "E"

-- | Display an equation and followed by a comma.
mathDisplay' :: LaTeXC r => r -> r
mathDisplay' e = mathDisplay $ e<>","

-- | Display an equation and followed by a full stop.
mathDisplay'' :: LaTeXC r => r -> r
mathDisplay'' e = mathDisplay $ e<>"."
       
infixl 9 ⎛
(⎛) :: Brakey b => (b->c) -> b -> c
f⎛x = f $ brak x

class Num b => Brakey b where brak :: b -> b
instance Brakey Int where brak = id
instance Brakey Double where brak = id
instance Monad m => Brakey (LaTeXT m ()) where brak = autoParens
