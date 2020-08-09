module Timeline.ID.RootTimeSpaceOrParent where

import Timeline.ID.TimeSpan (TimeSpanID)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.:), JsonDecodeError (TypeMismatch))
import Data.ArrayBuffer.Class (class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength)
import Control.Alternative ((<|>))
import Control.Monad.Error.Class (throwError)
import Test.QuickCheck (class Arbitrary)

newtype RootTimeSpaceOrParentID = RootTimeSpaceOrParentID (Maybe TimeSpanID)

derive instance genericRootTimeSpaceOrParentID :: Generic RootTimeSpaceOrParentID _

derive newtype instance eqRootTimeSpaceOrParentID :: Eq RootTimeSpaceOrParentID

derive newtype instance showRootTimeSpaceOrParentID :: Show RootTimeSpaceOrParentID

instance encodeJsonRootTimeSpaceOrParentID :: EncodeJson RootTimeSpaceOrParentID where
  encodeJson (RootTimeSpaceOrParentID mParent) = case mParent of
    Nothing -> encodeJson "root"
    Just parent -> "ownedBy" := parent ~> jsonEmptyObject

instance decodeJsonRootTimeSpaceOrParentID :: DecodeJson RootTimeSpaceOrParentID where
  decodeJson json = do
    let stringy = do
          s <- decodeJson json
          if s == "root" then pure (RootTimeSpaceOrParentID Nothing)
            else throwError $ TypeMismatch $ "String value not supported: " <> s
        objecty = do
          o <- decodeJson json
          RootTimeSpaceOrParentID <<< Just <$> o .: "ownedBy"
    stringy <|> objecty

derive newtype instance encodeArrayBufferRootTimeSpaceOrParentID :: EncodeArrayBuffer RootTimeSpaceOrParentID

derive newtype instance decodeArrayBufferRootTimeSpaceOrParentID :: DecodeArrayBuffer RootTimeSpaceOrParentID

derive newtype instance dynamicByteLengthRootTimeSpaceOrParentID :: DynamicByteLength RootTimeSpaceOrParentID

derive newtype instance arbitraryRootTimeSpaceOrParentID :: Arbitrary RootTimeSpaceOrParentID
