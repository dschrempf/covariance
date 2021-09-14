-- |
-- Module      :  Statistics.Covariance
-- Description :  Estimate covariance matrices from sample data
-- Copyright   :  (c) 2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Sep 14 13:02:15 2021.
module Statistics.Covariance
  ( -- * Shrinkage based estimators
    module Statistics.Covariance.LedoitWolf,
    module Statistics.Covariance.RaoBlackwellLedoitWolf,
    module Statistics.Covariance.OracleApproximatingShrinkage,
  )
where

import Statistics.Covariance.LedoitWolf
import Statistics.Covariance.OracleApproximatingShrinkage
import Statistics.Covariance.RaoBlackwellLedoitWolf
