module Borderless.Prelude
    ( -- * JSON
      aesonOptions
    , stripPrefix
    , labelModifier
    , dropUnderscores
    , camelToSnake
      -- * Text utilities
    , renderIntegral
      -- * Re-exports
    , module Data.Aeson
    , module Data.ByteString.Lazy
    , module Data.Map
    , module Data.String
    , module Data.Text
    , module Data.Time
    , module Data.Vector
    , module Data.Void
    , module GHC.Generics
    , module Numeric.Natural
    , Post201
    , module Servant.API
    , module Web.HttpApiData
      -- * Idempotency Key
    , IdempotencyKey(..)
    , newIdempotencyKey
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Void (Void)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Web.HttpApiData (ToHttpApiData(..), FromHttpApiData(..))

import Data.Aeson
    ( FromJSON(..)
    , Options(..)
    , SumEncoding(..)
    , ToJSON(..)
    , Value(..)
    , genericParseJSON
    , genericToJSON
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.=)
    )
import Servant.API
    ( Accept(..)
    , Capture
    , Delete
    , Get
    , Header'
    , JSON
    , MimeUnrender(..)
    , OctetStream
    , Optional
    , Patch
    , Post
    , Put
    , QueryParam
    , QueryParam'
    , ReqBody
    , Required
    , Strict
    , StdMethod(POST)
    , Verb
    , NoContent(..)
    , (:<|>)(..)
    , (:>)
    )

import Data.UUID (UUID)
import qualified Data.Aeson as A
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Int
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4

-- | POST that accepts 201 Created (used by most Borderless create endpoints)
type Post201 = Verb 'POST 201

-- | Idempotency key for API requests that require deduplication
newtype IdempotencyKey = IdempotencyKey { unIdempotencyKey :: UUID }
    deriving stock (Eq, Show)
    deriving newtype (FromJSON, ToJSON)

instance ToHttpApiData IdempotencyKey where
    toUrlPiece (IdempotencyKey uuid) = UUID.toText uuid

-- | Generate a new random idempotency key
newIdempotencyKey :: IO IdempotencyKey
newIdempotencyKey = IdempotencyKey <$> UUID.V4.nextRandom

dropUnderscores :: String -> String
dropUnderscores ('_' : cs) = dropTrailing cs
  where
    dropTrailing "_" = ""
    dropTrailing ""  = ""
    dropTrailing (c : rest) = c : dropTrailing rest
dropUnderscores s = s

labelModifier :: String -> String
labelModifier = lowercaseFirst . dropUnderscores
  where
    lowercaseFirst [] = []
    lowercaseFirst (c:cs) = Char.toLower c : cs

stripPrefix :: String -> String -> String
stripPrefix prefix string = suffix
  where
    suffix = case List.stripPrefix prefix string of
        Nothing -> string
        Just x  -> x

camelToSnake :: String -> String
camelToSnake = concatMap go
  where
    go c
        | Char.isUpper c = ['_', Char.toLower c]
        | otherwise      = [c]

aesonOptions :: Options
aesonOptions = A.defaultOptions
    { fieldLabelModifier = labelModifier
    , constructorTagModifier = labelModifier
    , omitNothingFields = True
    }

renderIntegral :: Integral number => number -> Text
renderIntegral number = TL.toStrict (Builder.toLazyText builder)
  where
    builder = Int.decimal number
