module Timeline.ID.ChildOrSiblingParent where

import Timeline.ID.TimeSpace (TimeSpaceID)
import Timeline.ID.Timeline (TimelineID)

import Prelude
import Data.Either (Either (..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (:=), (~>), jsonEmptyObject, (.:))
import Data.ArrayBuffer.Class (class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength)
import Control.Alternative ((<|>))
import Test.QuickCheck (class Arbitrary)

newtype ChildOrSiblingParentID = ChildOrSiblingParentID (Either TimelineID TimeSpaceID)

derive instance genericChildOrSiblingParentID :: Generic ChildOrSiblingParentID _

derive newtype instance eqChildOrSiblingParentID :: Eq ChildOrSiblingParentID

derive newtype instance showChildOrSiblingParentID :: Show ChildOrSiblingParentID

instance encodeJsonChildOrSiblingParentID :: EncodeJson ChildOrSiblingParentID where
  encodeJson (ChildOrSiblingParentID eCorS) = case eCorS of
    Left c -> "childOf" := c ~> jsonEmptyObject
    Right s -> "siblingOf" := s ~> jsonEmptyObject

instance decodeJsonChildOrSiblingParentID :: DecodeJson ChildOrSiblingParentID where
  decodeJson json = do
    o <- decodeJson json
    (ChildOrSiblingParentID <<< Left <$> o .: "childOf") <|> (ChildOrSiblingParentID <<< Right <$> o .: "siblingOf")

derive newtype instance encodeArrayBufferChildOrSiblingParentID :: EncodeArrayBuffer ChildOrSiblingParentID

derive newtype instance decodeArrayBufferChildOrSiblingParentID :: DecodeArrayBuffer ChildOrSiblingParentID

derive newtype instance dynamicByteLengthChildOrSiblingParentID :: DynamicByteLength ChildOrSiblingParentID

derive newtype instance arbitraryChildOrSiblingParentID :: Arbitrary ChildOrSiblingParentID
