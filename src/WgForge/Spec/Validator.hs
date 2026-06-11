-- | Public validation API: 'validateNetwork' runs every check and
--   accumulates all errors. The individual checks live in
--   "WgForge.Spec.Validator.Internal".
module WgForge.Spec.Validator (
  validateNetwork,
) where

import WgForge.Spec.Validator.Internal
