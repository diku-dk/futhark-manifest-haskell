{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Futhark.Manifest
import Test.QuickCheck.Instances.Text ()
import Test.Tasty
import Test.Tasty.QuickCheck

-- These instances may generate manifests that are nonsensical in that
-- the entry points likely refer to nonexistent types.  This is fine
-- for testing serialisation.

instance Arbitrary ArrayOps where
  arbitrary =
    ArrayOps
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary RecordField where
  arbitrary = RecordField <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary RecordOps where
  arbitrary = RecordOps <$> arbitrary <*> arbitrary

instance Arbitrary SumVariant where
  arbitrary = SumVariant <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SumOps where
  arbitrary = SumOps <$> arbitrary <*> arbitrary

instance Arbitrary OpaqueArrayOps where
  arbitrary =
    OpaqueArrayOps
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary RecordArrayOps where
  arbitrary =
    RecordArrayOps
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary OpaqueExtraOps where
  arbitrary =
    oneof
      [ OpaqueRecord <$> arbitrary,
        OpaqueSum <$> arbitrary,
        OpaqueArray <$> arbitrary,
        OpaqueRecordArray <$> arbitrary
      ]

instance Arbitrary OpaqueOps where
  arbitrary = OpaqueOps <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Type where
  arbitrary =
    oneof
      [ TypeArray <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        TypeOpaque <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Arbitrary Output where
  arbitrary = Output <$> arbitrary <*> arbitrary

instance Arbitrary Input where
  arbitrary = Input <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary EntryPoint where
  arbitrary = EntryPoint <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Manifest where
  arbitrary = Manifest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

jsonTests :: TestTree
jsonTests =
  testGroup
    "JSON"
    [ testProperty "manifestFromJSON . manifestToJSON = id" $
        \v -> manifestFromJSON (manifestToJSON v) == Just v
    ]

allTests :: TestTree
allTests =
  testGroup "" [jsonTests]

main :: IO ()
main = defaultMain allTests
