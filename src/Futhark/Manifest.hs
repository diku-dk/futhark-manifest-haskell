{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- | C manifest data structure and serialisation to JSON.
--
-- A manifest contains machine-readable information about the API of
-- the compiled Futhark program.  Specifically which entry points are
-- available, which types are exposed, and what their C names are.
-- This module documentation is not intended as a full description of
-- the Futhark C API - you will need to consult the Futhark User's
-- Guide to understand most of the information here.
--
-- The type aliases are purely informative and do not actually enforce
-- correct usage.  They are present only because most of the
-- information here is ultimately just text.
module Futhark.Manifest
  ( -- * Type aliases
    CFuncName,
    CTypeName,
    TypeName,

    -- * Manifest
    Manifest (..),
    Input (..),
    Output (..),
    EntryPoint (..),
    Type (..),
    ArrayOps (..),
    RecordField (..),
    RecordOps (..),
    SumVariant (..),
    SumOps (..),
    OpaqueOps (..),
    manifestToJSON,
    manifestFromJSON,
  )
where

import Control.Applicative
import Control.Monad (guard)
import Data.Aeson (ToJSON (..), object, (.!=), (.:), (.:?))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (bimap)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Text.Lazy (toStrict)

-- | The name of a C function.
type CFuncName = T.Text

-- | The name of a C type (often of the form @"struct foo*"@).
type CTypeName = T.Text

-- | The name of a Futhark-level type.  This may be an array type
-- (without sizes, just empty brackets), a primitive type, or another
-- string denoting an opaque type.  The latter must have a
-- corresponding entry in 'manifestTypes'.
type TypeName = T.Text

-- | Manifest info for an entry point parameter.
data Input = Input
  { inputName :: T.Text,
    inputType :: TypeName,
    inputUnique :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Manifest info for an entry point return value.
data Output = Output
  { outputType :: TypeName,
    outputUnique :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Manifest info for an entry point.
data EntryPoint = EntryPoint
  { entryPointCFun :: CFuncName,
    entryPointTuningParams :: [T.Text],
    entryPointOutputs :: [Output],
    entryPointInputs :: [Input]
  }
  deriving (Eq, Ord, Show)

-- | The names of the C functions implementing the operations on some
-- array type.
data ArrayOps = ArrayOps
  { arrayFree :: CFuncName,
    arrayShape :: CFuncName,
    arrayValues :: CFuncName,
    arrayNew :: CFuncName
  }
  deriving (Eq, Ord, Show)

-- | Information about a record field.
data RecordField = RecordField
  { -- | The original name of the field.  This may be a name that is
    -- not a valid C identifier.
    recordFieldName :: T.Text,
    -- | The type of the field.
    recordFieldType :: TypeName,
    -- | The name of the projection function.
    recordFieldProject :: CFuncName
  }
  deriving (Eq, Ord, Show)

-- | Some opaque types are records, from which we can extract fields,
-- and also construct them from values for their fields.  Beyond that,
-- they support the usual opaque operations.  These record facilities
-- can be ignored if you wish, and the types treated as ordinary
-- opaque types.
data RecordOps = RecordOps
  { -- | Note that the ordering of fields here is semantically
    -- significant - it is also the order that the "new" function
    -- expects.
    recordFields :: [RecordField],
    recordNew :: CFuncName
  }
  deriving (Eq, Ord, Show)

-- | Information about a variant of a sum type.
data SumVariant = SumVariant
  { -- | The name of the constructor. This may be a name that is not a
    -- valid C identifier.
    sumVariantName :: T.Text,
    -- | The payload of this variant; also corresponding to the
    -- arguments of the constructor and destructor functions.
    sumVariantPayload :: [TypeName],
    sumVariantConstruct :: CFuncName,
    -- | Note that despite the name, "destruction" does not entail
    -- freeing the sum type value.
    sumVariantDestruct :: CFuncName
  }
  deriving (Eq, Ord, Show)

-- | Some opaque types are sum types, from which we can (try to)
-- extract the payload of a constructor, as well as construct them
-- from payloads. As with records, we can ignore these facilities and
-- simply treat them as completely opaque.
data SumOps = SumOps
  { sumVariants :: [SumVariant],
    -- | This function returns an integer that identifies which
    -- variant a value is an instance of. This integer is a valid
    -- index in 'sumVariants'.
    sumVariant :: CFuncName
  }
  deriving (Eq, Ord, Show)

-- | The names of the C functions implementing the operations on some
-- opaque type.
data OpaqueOps = OpaqueOps
  { opaqueFree :: CFuncName,
    opaqueStore :: CFuncName,
    opaqueRestore :: CFuncName
  }
  deriving (Eq, Ord, Show)

-- | Manifest info for a non-scalar type. Scalar types are not part of
-- the manifest for a program. Although this representation allows a
-- type to be both a a record and a sum type, this will never actually
-- happen.
data Type
  = -- | ctype, Futhark elemtype, rank.
    TypeArray CTypeName TypeName Int ArrayOps
  | TypeOpaque CTypeName OpaqueOps (Maybe RecordOps) (Maybe SumOps)
  deriving (Eq, Ord, Show)

-- | A manifest for a compiled program.
data Manifest = Manifest
  { -- | A mapping from Futhark entry points to how they are
    -- represented in C.
    manifestEntryPoints :: M.Map T.Text EntryPoint,
    -- | A mapping from Futhark type name to how they are represented
    -- at the C level.  Should not contain any of the primitive scalar
    -- types.  For array types, these have empty dimensions,
    -- e.g. @[]i32@.
    manifestTypes :: M.Map TypeName Type,
    -- | The compiler backend used to
    -- compile the program, e.g. @c@.
    manifestBackend :: T.Text,
    -- | The version of the compiler used to compile the program.
    manifestVersion :: T.Text
  }
  deriving (Eq, Ord, Show)

instance JSON.ToJSON ArrayOps where
  toJSON (ArrayOps free shape values new) =
    object
      [ ("free", toJSON free),
        ("shape", toJSON shape),
        ("values", toJSON values),
        ("new", toJSON new)
      ]

instance JSON.ToJSON RecordField where
  toJSON (RecordField name typename project) =
    object
      [ ("name", toJSON name),
        ("type", toJSON typename),
        ("project", toJSON project)
      ]

instance JSON.ToJSON RecordOps where
  toJSON (RecordOps fields new) =
    object
      [ ("fields", toJSON fields),
        ("new", toJSON new)
      ]

instance JSON.ToJSON SumVariant where
  toJSON (SumVariant name payload construct destruct) =
    object
      [ ("name", toJSON name),
        ("payload", toJSON payload),
        ("construct", toJSON construct),
        ("destruct", toJSON destruct)
      ]

instance JSON.ToJSON SumOps where
  toJSON (SumOps variants variant) =
    object
      [ ("variants", toJSON variants),
        ("variant", toJSON variant)
      ]

instance JSON.ToJSON OpaqueOps where
  toJSON (OpaqueOps free store restore) =
    object $
      [ ("free", toJSON free),
        ("store", toJSON store),
        ("restore", toJSON restore)
      ]

instance JSON.ToJSON Manifest where
  toJSON (Manifest entry_points types backend version) =
    object
      [ ("backend", toJSON backend),
        ("version", toJSON version),
        ( "entry_points",
          object $ map (bimap JSON.fromText onEntryPoint) $ M.toList entry_points
        ),
        ( "types",
          object $ map (bimap JSON.fromText onType) $ M.toList types
        )
      ]
    where
      onEntryPoint (EntryPoint cfun tuning_params outputs inputs) =
        object
          [ ("cfun", toJSON cfun),
            ("tuning_params", toJSON tuning_params),
            ("outputs", toJSON $ map onOutput outputs),
            ("inputs", toJSON $ map onInput inputs)
          ]

      onOutput (Output t u) =
        object
          [ ("type", toJSON t),
            ("unique", toJSON u)
          ]

      onInput (Input p t u) =
        object
          [ ("name", toJSON p),
            ("type", toJSON t),
            ("unique", toJSON u)
          ]

      onType (TypeArray t et rank ops) =
        object
          [ ("kind", "array"),
            ("ctype", toJSON t),
            ("rank", toJSON rank),
            ("elemtype", toJSON et),
            ("ops", toJSON ops)
          ]
      onType (TypeOpaque t ops record sumops) =
        object $
          [ ("kind", "opaque"),
            ("ctype", toJSON t),
            ("ops", toJSON ops)
          ]
            ++ maybeToList (("record",) . toJSON <$> record)
            ++ maybeToList (("sum",) . toJSON <$> sumops)

instance JSON.FromJSON ArrayOps where
  parseJSON = JSON.withObject "ArrayOps" $ \v ->
    ArrayOps <$> v .: "free" <*> v .: "shape" <*> v .: "values" <*> v .: "new"

instance JSON.FromJSON RecordField where
  parseJSON = JSON.withObject "RecordField" $ \v ->
    RecordField <$> v .: "name" <*> v .: "type" <*> v .: "project"

instance JSON.FromJSON RecordOps where
  parseJSON = JSON.withObject "RecordOps" $ \v ->
    RecordOps <$> v .: "fields" <*> v .: "new"

instance JSON.FromJSON SumVariant where
  parseJSON = JSON.withObject "SumVariant" $ \v ->
    SumVariant
      <$> v .: "name"
      <*> v .: "payload"
      <*> v .: "construct"
      <*> v .: "destruct"

instance JSON.FromJSON SumOps where
  parseJSON = JSON.withObject "SumOps" $ \v ->
    SumOps <$> v .: "variants" <*> v .: "variant"

instance JSON.FromJSON OpaqueOps where
  parseJSON = JSON.withObject "OpaqueOps" $ \v ->
    OpaqueOps <$> v .: "free" <*> v .: "store" <*> v .: "restore"

instance JSON.FromJSON EntryPoint where
  parseJSON = JSON.withObject "EntryPoint" $ \v ->
    EntryPoint
      <$> v .: "cfun"
      <*> v .: "tuning_params"
      <*> v .: "outputs"
      <*> v .: "inputs"

instance JSON.FromJSON Output where
  parseJSON = JSON.withObject "Output" $ \v ->
    Output <$> v .: "type" <*> v .: "unique"

instance JSON.FromJSON Input where
  parseJSON = JSON.withObject "Input" $ \v ->
    Input <$> v .: "name" <*> v .: "type" <*> v .: "unique"

instance JSON.FromJSON Type where
  parseJSON = JSON.withObject "Type" $ \ty -> pArray ty <|> pOpaque ty
    where
      pArray ty = do
        guard . (== ("array" :: T.Text)) =<< (ty .: "kind")
        TypeArray
          <$> ty .: "ctype"
          <*> ty .: "elemtype"
          <*> ty .: "rank"
          <*> ty .: "ops"
      pOpaque ty = do
        guard . (== ("opaque" :: T.Text)) =<< (ty .: "kind")
        TypeOpaque <$> ty .: "ctype" <*> ty .: "ops" <*> ty .:? "record" <*> ty .:? "sum"

instance JSON.FromJSON Manifest where
  parseJSON = JSON.withObject "Manifest" $ \v ->
    Manifest
      <$> v .: "entry_points"
      <*> v .: "types"
      <*> v .: "backend"
      <*> v .:? "version" .!= "" -- Temporary workaround for older manifests.

-- | Serialise a manifest to JSON.
manifestToJSON :: Manifest -> T.Text
manifestToJSON = toStrict . encodeToLazyText

-- | Read a manifest from JSON.  Returns 'Nothing' if the text does
-- not describe a 'Manifest'.
manifestFromJSON :: T.Text -> Maybe Manifest
manifestFromJSON = JSON.decode . toLazyByteString . encodeUtf8Builder
