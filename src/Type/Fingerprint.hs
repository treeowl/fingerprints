{- |
Type-indexed type fingerprints.

=== Warning

The fingerprints calculated by this package are //not always// the same
as the ones calculated by GHC. It is currently impossible to offer this
interface with that guarantee; that may change in the future.
-}
module Type.Fingerprint
  (
    Fingerprint
  , Fingerprinted
  , fingerprint
  , withFingerprinted
  , fingerprintOf
  , mkFpApp
  , mkFpFun
  , eqFingerprint
  , typeRepFingerprint
  , SomeFingerprint (SomeFingerprint)
  , someFingerprint
  , someTypeRepFingerprint
  ) where

import Type.Fingerprint.Internal
