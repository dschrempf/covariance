-- |
-- Module      :  Statistics.Shrinkage
-- Description :  Well-conditioned estimation of large-dimensional covariance matrices
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Sep  9 14:08:26 2021.
module Statistics.Shrinkage
  ( covariance,
  )
where

import qualified Numeric.LinearAlgebra as L

-- | Shrinkage based covariance estimator.
--
-- See Ledoit, O., & Wolf, M., A well-conditioned estimator for
-- large-dimensional covariance matrices, Journal of Multivariate Analysis,
-- 88(2), 365–411 (2004). http://dx.doi.org/10.1016/s0047-259x(03)00096-4.
covariance ::
  -- | Sample data matrix of dimension \(n \times p\), where \(n\) is the number
  -- of samples (rows), and \(p\) is the number of parameters (columns).
  L.Matrix Double ->
  L.Matrix Double
covariance xs = undefined

-- Adjusted Frobenius norm (p 376).
frobenius :: L.Matrix Double -> L.Matrix Double -> Double
frobenius xs ys
  | xsRows /= xsCols = error "frobenius: Left matrix is not square."
  | ysRows /= ysCols = error "frobenius: Right matrix is not square."
  | xsRows /= ysRows = error "frobenius: Matrices have different size."
  | otherwise = recip (fromIntegral xsRows) * trace (xs L.<> L.tr' ys)
  where xsRows = L.rows xs
        xsCols = L.cols xs
        ysRows = L.rows ys
        ysCols = L.cols ys
        trace = L.sumElements . L.takeDiag

-- Estimator of mu (lemma 3.2).
muE ::
  -- Sample covariance matrix.
  L.Matrix Double ->
  Double
muE sigma = undefined

-- Estimator of d2 (lemma 3.3).
d2E ::
  -- Sample covariance matrix.
  L.Matrix Double ->
  -- Estimate of mu.
  Double ->
  Double
d2E sigma mu = undefined

-- Estimator of b2 (lemma 3.4).
b2E ::
  -- Data matrix.
  L.Matrix Double ->
  -- Sample covariance matrix.
  L.Matrix Double ->
  -- Estimate of d2.
  Double ->
  Double
b2E xs sigma d2 = undefined

-- Estimator of a2 (lemma 3.5).
a2E ::
  -- Estimate of d2.
  Double ->
  -- Estimate of b2.
  Double ->
  Double
a2E d2 b2 = d2 - b2

covariance' ::
  -- Identity matrix.
  L.Matrix Double ->
  -- Sample covariance matrix.
  L.Matrix Double ->
  -- Estimate of mu.
  Double ->
  -- Estimate of d2.
  Double ->
  -- Estimate of b2.
  Double ->
  -- Estimate of a2.
  Double ->
  L.Matrix Double
covariance' iN sigma mu d2 b2 a2 = L.scale (b2 / d2 * mu) iN + L.scale (a2 / d2) sigma
