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

import Data.Foldable
import qualified Numeric.LinearAlgebra as L

-- TODO: Provide version using 'Either'.

-- TODO: Proofreading and tests, there is a dimensionality problem!

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
covariance xs
  | n < 2 = error "covariance: Need more than one sample."
  | otherwise = covariance' im sigma mu d2 b2 a2
  where n = L.rows xs
        p = L.cols xs
        sigma = L.scale (recip $ fromIntegral n) $ xs L.<> L.tr' xs
        im = L.ident p
        mu = muE im sigma
        d2 = d2E im sigma mu
        b2 = b2E xs sigma d2
        a2 = a2E d2 b2

-- Inner product for symmetric matrices based on an adjusted Frobenius norm (p
-- 376).
--
-- NOTE: This function is commutative (and therefor qualified as an inner
-- product) for symmetric matrices only, and not in the general case.
frobenius :: L.Matrix Double -> L.Matrix Double -> Double
frobenius xs ys
  | xsRows /= xsCols = error "frobenius: Left matrix is not square."
  | ysRows /= ysCols = error "frobenius: Right matrix is not square."
  | xsRows /= ysRows = error "frobenius: Matrices have different size."
  | otherwise = recip (fromIntegral xsRows) * trace (xs L.<> L.tr' ys)
  where
    xsRows = L.rows xs
    xsCols = L.cols xs
    ysRows = L.rows ys
    ysCols = L.cols ys
    trace = L.sumElements . L.takeDiag

-- Estimator of mu (Lemma 3.2).
muE ::
  -- Identity matrix.
  L.Matrix Double ->
  -- Sample covariance matrix.
  L.Matrix Double ->
  Double
-- NOTE: Strictly speaking, 'flip' is required here to fulfill the formulas
-- given in Lemma 3.2. However, both matrices are symmetric and so, it can be
-- omitted. See 'frobenius'.
muE = frobenius

-- Estimator of d2 (Lemma 3.3).
d2E ::
  -- Identity matrix.
  L.Matrix Double ->
  -- Sample covariance matrix.
  L.Matrix Double ->
  -- Estimate of mu.
  Double ->
  Double
d2E im sigma mu = frobenius m m
  where
    m = sigma - L.scale mu im

-- Estimator of b2 (Lemma 3.4).
b2E ::
  -- Data matrix.
  L.Matrix Double ->
  -- Sample covariance matrix.
  L.Matrix Double ->
  -- Estimate of d2.
  Double ->
  Double
b2E xs sigma = min b2m
  where
    n = fromIntegral $ L.rows sigma
    -- NOTE: The authors use a transposed data matrix. They refer to one out of
    -- n columns, each with p rows. Here, we have one out of n rows, each with p
    -- columns.
    --
    -- Long story short. Each y is an observation, a vector of length p.
    ys =
      [ frobenius d d
        | y <- L.toRows xs,
          let d = (L.asColumn y L.<> L.asRow y) - sigma
      ]
    b2m = recip (n * n) * foldl' (+) 0 ys

-- Estimator of a2 (Lemma 3.5).
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
covariance' im sigma mu d2 b2 a2 = L.scale (b2 / d2 * mu) im + L.scale (a2 / d2) sigma
