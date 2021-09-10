-- |
-- Module      :  Statistics.Covariance.LedoitWolf
-- Description :  Shrinkage based covariance estimator by Ledoit and Wolf
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Sep  9 14:08:26 2021.
module Statistics.Covariance.LedoitWolf
  ( ledoitWolf,
  )
where

import Data.Foldable
import qualified Numeric.LinearAlgebra as L
import Statistics.Covariance.Internal.Tools

-- | Shrinkage based covariance estimator by Ledoit and Wolf.
--
-- See Ledoit, O., & Wolf, M., A well-conditioned estimator for
-- large-dimensional covariance matrices, Journal of Multivariate Analysis,
-- 88(2), 365–411 (2004). http://dx.doi.org/10.1016/s0047-259x(03)00096-4.
--
-- Return 'Left' if
--
-- - dimensions do not match;
--
-- - only one sample is available.
--
-- - no parameters are available.
--
-- NOTE: This function may still fail due to partial library functions.
ledoitWolf ::
  -- | Sample data matrix of dimension \(n \times p\), where \(n\) is the number
  -- of samples (rows), and \(p\) is the number of parameters (columns).
  L.Matrix Double ->
  Either String (L.Herm Double)
ledoitWolf xs
  | n < 2 = Left "ledoitWolf: Need more than one sample."
  | p < 1 = Left "ledoitWolf: Need at least one parameter."
  | otherwise = do
    d2 <- d2E im sigma mu
    b2 <- b2E xsCentered sigma d2
    let rho = b2 / d2
    -- The Ledoit and Wolf shrinkage estimator of the covariance matrix
    -- (Equation 14). However, a different, more general formula avoiding a2 is
    -- used. See Equation (4) in Chen2010b.
    Right $ shrinkWith rho sigma mu im
  where
    n = L.rows xs
    p = L.cols xs
    (means, sigma) = L.meanCov xs
    xsCentered = centerWith means xs
    im = L.trustSym $ L.ident p
    mu = muE sigma

-- Inner product for symmetric matrices based on an adjusted Frobenius norm (p
-- 376).
--
-- NOTE: This function is commutative (and therefor qualified as an inner
-- product) for symmetric matrices only, and not in the general case.
frobenius :: L.Matrix Double -> L.Matrix Double -> Either String Double
frobenius xs ys
  | xsRows /= xsCols = Left "frobenius: Left matrix is not square."
  | ysRows /= ysCols = Left "frobenius: Right matrix is not square."
  | xsRows /= ysRows = Left "frobenius: Matrices have different size."
  | otherwise = Right $ recip (fromIntegral xsRows) * trace (xs L.<> L.tr' ys)
  where
    xsRows = L.rows xs
    xsCols = L.cols xs
    ysRows = L.rows ys
    ysCols = L.cols ys

-- Estimator of mu (Lemma 3.2).
muE ::
  -- Sample covariance matrix.
  L.Herm Double ->
  Double
-- Avoid matrix multiplication in Frobenius norm.
muE sigma' = recip xsRows * trace sigma
  where
    sigma = L.unSym sigma'
    xsRows = fromIntegral $ L.rows sigma

-- Estimator of d2 (Lemma 3.3).
d2E ::
  -- Identity matrix.
  L.Herm Double ->
  -- Sample covariance matrix.
  L.Herm Double ->
  -- Estimate of mu.
  Double ->
  Either String Double
d2E im sigma mu = frobenius m m
  where
    m = L.unSym sigma - L.scale mu (L.unSym im)

-- Estimator of b2 (Lemma 3.4).
b2E ::
  -- Data matrix.
  L.Matrix Double ->
  -- Sample covariance matrix.
  L.Herm Double ->
  -- Estimate of d2.
  Double ->
  Either String Double
b2E xs sigma d2 = do
  -- NOTE: The authors use a transposed data matrix. They refer to one out of
  -- n columns, each with p rows. Here, we have one out of n rows, each with p
  -- columns.
  --
  -- Long story short. Each y is an observation, a vector of length p.
  ys <-
    sequence
      [ frobenius d d
        | y <- L.toRows xs,
          let d = (L.asColumn y L.<> L.asRow y) - L.unSym sigma
      ]
  let b2m = recip (n * n) * foldl' (+) 0 ys
  Right $ min b2m d2
  where
    n = fromIntegral $ L.rows xs
