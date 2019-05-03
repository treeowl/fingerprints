{-# language ScopedTypeVariables, TypeInType, GADTs, RoleAnnotations, TypeApplications, RankNTypes,
      MultiParamTypeClasses, TypeOperators, TypeFamilies, FlexibleContexts, FlexibleInstances,
      UndecidableInstances, AllowAmbiguousTypes, MagicHash, GeneralizedNewtypeDeriving,
      PatternSynonyms, ViewPatterns #-}
module Type.Fingerprint.Internal
  (
    SomeFingerprint (SomeFingerprint', SomeFingerprint)
  , module Type.Fingerprint.Internal
  ) where

import GHC.Fingerprint (fingerprintFingerprints)
import qualified GHC.Fingerprint as F
import Data.Type.Equality
import Type.Reflection
import qualified Type.Reflection.Unsafe as TU
import Data.Kind
import GHC.Exts (TYPE, RuntimeRep (..))
import Unsafe.Coerce
import Data.Binary

newtype Fingerprint (a :: k) = Fingerprint F.Fingerprint
  deriving Show

instance Eq (Fingerprint a) where
  _ == _ = True

instance Ord (Fingerprint a) where
  compare _ _ = EQ

instance Fingerprinted a => Binary (Fingerprint a) where
  put (Fingerprint p) = put p
  get = do
    fp <- get
    if fp == fpt 
      then pure $ Fingerprint fp
      else fail "Fingerprint mismatch on deserialization."
    where Fingerprint fpt = fingerprint @a

type role Fingerprint nominal

-- We pretend very hard that this is defined
--
--   data SomeFingerprint = forall a. SomeFingerprint !(Fingerprint a)
--
-- but we use a newtype for performance.
newtype SomeFingerprint = SomeFingerprint' F.Fingerprint
  deriving (Show, Binary)

pattern SomeFingerprint :: () => forall k (a :: k). Fingerprint a -> SomeFingerprint
pattern SomeFingerprint fp <- ((\(SomeFingerprint' x) -> Fingerprint x) -> fp)
  where
    SomeFingerprint (Fingerprint fp) = SomeFingerprint' fp
{-# COMPLETE SomeFingerprint #-}


class Fingerprinted (a :: k) where
  fingerprint# :: Fingerprint a

fingerprint :: Fingerprinted a => Fingerprint a
fingerprint = fingerprint#

fingerprintOf :: Fingerprinted a => a -> Fingerprint a
fingerprintOf _ = fingerprint

mkFpFun :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (arg :: TYPE r1) (res :: TYPE r2).
         Fingerprint arg -> Fingerprint res -> Fingerprint (arg -> res :: Type)
mkFpFun fpr_a fpr_b =
--   (fingerprint) `mkFpApp` fpr_a `mkFpApp` fpr_b  -- see https://gitlab.haskell.org/ghc/ghc/issues/16627
  (Fingerprint . TU.typeRepFingerprint $ typeRep @(->)) `mkFpApp` fpr_a `mkFpApp` fpr_b

mkFpApp :: forall k1 (a :: k1 -> k2) (b :: k1).
         Fingerprint a -> Fingerprint b -> Fingerprint (a b)
mkFpApp (Fingerprint fpr1) (Fingerprint fpr2) = Fingerprint $ fingerprintFingerprints [fpr1, fpr2]

instance Fingerprinted' (GetStyle a) a => Fingerprinted (a :: k) where
  fingerprint# = fingerprint' @_ @(GetStyle a)

data Style = ConS | AppS

type family GetStyle (a :: k) :: Style where
  GetStyle ((a :: k1 -> k2) (b :: k1)) = 'AppS
  GetStyle con = 'ConS

class Fingerprinted' (s :: Style) (a :: k) where
  fingerprint' :: Fingerprint a
instance Typeable a => Fingerprinted' 'ConS (a :: k) where
  fingerprint' = Fingerprint . TU.typeRepFingerprint $ typeRep @a
instance (Fingerprinted a, Fingerprinted b) => Fingerprinted' 'AppS ((a :: k1 -> k2) (b :: k1)) where
  fingerprint' = mkFpApp (fingerprint @a) (fingerprint @b)

typeRepFingerprint :: TypeRep a -> Fingerprint a
typeRepFingerprint (App f a) = mkFpApp (typeRepFingerprint f) (typeRepFingerprint a)
typeRepFingerprint x = Fingerprint . TU.typeRepFingerprint $ x

someTypeRepFingerprint :: SomeTypeRep -> SomeFingerprint
someTypeRepFingerprint = SomeFingerprint' . TU.someTypeRepFingerprint

someFingerprint :: forall proxy a. Fingerprinted a => proxy a -> SomeFingerprint
someFingerprint _
  | Fingerprint fpr <- fingerprint @a
  = SomeFingerprint' fpr

eqFingerprint
  :: forall k1 k2 (a :: k1) (b :: k2).
     Fingerprint a -> Fingerprint b -> Maybe (a :~~: b)
eqFingerprint (Fingerprint fpr1) (Fingerprint fpr2)
  | fpr1 == fpr2 = Just (unsafeCoerce (HRefl :: a :~~: a))
  | otherwise = Nothing

withFingerprinted
  :: forall k (a :: k) rep (r :: TYPE rep) .
     Fingerprint a -> (Fingerprinted a => r) -> r
withFingerprinted fp k = unsafeCoerce k' fp
  where k' :: Gift a r
        k' = Gift k

-- | A helper to satisfy the type checker in 'withTypeable'.
newtype Gift a (r :: TYPE rep) = Gift (Fingerprinted a => r)
