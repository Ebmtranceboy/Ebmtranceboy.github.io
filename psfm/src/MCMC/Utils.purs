module MCMC.Utils where

import Prelude

import Data.Array (length)
import Data.Int (toNumber)
import Math (sqrt)
import Mecanism ((!!))

type Summary = {expectation :: Number, dispersion :: Number}

welfordSummary :: Array Number -> Summary
welfordSummary f =
  let iteration n summary@{expectation: e, dispersion: v} =
        if n == length f
          then {expectation: e, dispersion: v / toNumber (length f - 1)}
          else
            let fn = f !! n
                delta = fn - e
                e' = e + delta / (toNumber $ n + 2)
            in iteration (n+1) { expectation: e'
                               , dispersion: v + delta * (fn - e')
                               }
    in iteration 0 {expectation: 0.0, dispersion: 0.0}

computeMCstats :: Array Number -> Summary
computeMCstats f =
  let {expectation, dispersion: v} = welfordSummary f
  in {expectation, dispersion: sqrt (v / toNumber (length f))}
