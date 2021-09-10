-- |
-- Module      :  Spec
-- Description :  Covariance test suite
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Sep  9 21:59:03 2021.
module Main
  ( main,
  )
where

import Data.Either
import qualified Numeric.LinearAlgebra as L
import Statistics.Covariance.LedoitWolf
import Statistics.Covariance.RaoBlackwellLedoitWolf
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain unitTests

emptyM :: L.Matrix Double
emptyM = L.fromLists [[]]

oneSampleM :: L.Matrix Double
oneSampleM = L.fromLists [[1, 2, 3, 4, 5]]

type Estimator = (L.Matrix Double -> Either String (L.Herm Double), String)

unitTestsForEstimator :: Estimator -> [TestTree]
unitTestsForEstimator (e, m) =
  [ testCase (m <> " fails on empty data matrices.") $
      assertEqual "" True (isLeft $ e emptyM),
    testCase (m <> " fails on one sample data matrices.") $
      assertEqual "" True (isLeft $ raoBlackwellLedoitWolf oneSampleM)
  ]

unitTestsForEstimators :: [Estimator] -> [TestTree]
unitTestsForEstimators = concatMap unitTestsForEstimator

estimators :: [Estimator]
estimators =
  [ (ledoitWolf, "ledoitWolf"),
    (raoBlackwellLedoitWolf, "raoBlackwellLedoitWolf")
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" $ unitTestsForEstimators estimators
