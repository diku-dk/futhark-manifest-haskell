{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

-- | The names of the C functions implementing the operations on some
-- opaque type.
data OpaqueOps = OpaqueOps
  { opaqueFree :: CFuncName,
    opaqueStore :: CFuncName,
    opaqueRestore :: CFuncName
  }
  deriving (Eq, Ord, Show)

-- | Manifest info for a non-scalar type.  Scalar types are not part
-- of the manifest for a program.
data Type
  = -- | ctype, Futhark elemtype, rank.
    TypeArray CTypeName TypeName Int ArrayOps
  | TypeOpaque CTypeName OpaqueOps (Maybe RecordOps)
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
      onEntryPoint (EntryPoint cfun outputs inputs) =
        object
          [ ("cfun", toJSON cfun),
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
      onType (TypeOpaque t ops record) =
        object $
          [ ("kind", "opaque"),
            ("ctype", toJSON t),
            ("ops", toJSON ops)
          ]
            ++ maybeToList (("record",) . toJSON <$> record)

instance JSON.FromJSON ArrayOps where
  parseJSON = JSON.withObject "ArrayOps" $ \v ->
    ArrayOps <$> v .: "free" <*> v .: "shape" <*> v .: "values" <*> v .: "new"

instance JSON.FromJSON RecordField where
  parseJSON = JSON.withObject "RecordField" $ \v ->
    RecordField <$> v .: "name" <*> v .: "type" <*> v .: "project"

instance JSON.FromJSON RecordOps where
  parseJSON = JSON.withObject "RecordOps" $ \v ->
    RecordOps <$> v .: "fields" <*> v .: "new"

instance JSON.FromJSON OpaqueOps where
  parseJSON = JSON.withObject "OpaqueOps" $ \v ->
    OpaqueOps <$> v .: "free" <*> v .: "store" <*> v .: "restore"

instance JSON.FromJSON EntryPoint where
  parseJSON = JSON.withObject "EntryPoint" $ \v ->
    EntryPoint <$> v .: "cfun" <*> v .: "outputs" <*> v .: "inputs"

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
        TypeOpaque <$> ty .: "ctype" <*> ty .: "ops" <*> ty .:? "record"

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
