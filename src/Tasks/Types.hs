{-# LANGUAGE TypeSynonymInstances #-}
-- Necessary to implement a binary instance for DateTime
-- |
-- Module: Tasks.Types
--
-- This module contains convenience functions useful in
-- handling and creating bytestrings in other parts of
-- this package, as well as the Metadata and Priority types

module Tasks.Types
   (
     bs
   , bsToString
   , bsEmpty
   , Priority(..)
   , Metadata(..)

   , mdEditNotes
   , mdEditPriority
   , mdEditCompleted
   , mdEditDue

   , metadata
   ) where

import Control.Monad(liftM)
import qualified Data.ByteString as BW
import Data.Binary
import Data.Char(chr, ord)
import Data.DateTime(DateTime, toSeconds, fromSeconds)
import Data.Maybe

convertWord8ToChar :: Word8 -> Char
convertWord8ToChar = chr . fromIntegral

convertCharToWord8 :: Char -> Word8
convertCharToWord8 = fromIntegral . ord

stringToWByteString :: String -> BW.ByteString
stringToWByteString = BW.pack . map convertCharToWord8

wByteStringToString :: BW.ByteString -> String
wByteStringToString = map convertWord8ToChar . BW.unpack

-- | Create a ByteString given a String
bs :: String -> BW.ByteString
bs = stringToWByteString

-- | Convert a ByteString to a String
bsToString :: BW.ByteString -> String
bsToString = wByteStringToString

-- | Check whether a given ByteString is empty
bsEmpty :: BW.ByteString -> Bool
bsEmpty = (==0) . length . BW.unpack

instance Binary DateTime where
   put = put . toSeconds
   get = liftM fromSeconds (get :: Get Integer)

data Priority = Low | Medium | High deriving (Show, Read, Eq, Ord, Bounded, Enum)

instance Binary Priority where
   get = liftM toEnum get
   put = put . fromEnum

-- | The metadata type, encapsulating optional notes,
-- a priority, a completion status, and an optional due date
data Metadata = Metadata { mdNotes :: Maybe BW.ByteString
                         , mdPriority :: Priority
                         , mdCompleted :: Bool
                         , mdDue :: Maybe DateTime } deriving (Show, Read, Eq)

-- | Edit a given metadata's notes
mdEditNotes :: Metadata -> Maybe BW.ByteString -> Metadata
mdEditNotes md mnts = md { mdNotes = mnts }

-- | Edit a given metadata's priority
mdEditPriority :: Metadata -> Priority -> Metadata
mdEditPriority md pr = md { mdPriority = pr }

-- | Edit a given metadata's completion status.
mdEditCompleted :: Metadata -> Bool -> Metadata
mdEditCompleted md c = md { mdCompleted = c }

-- | Edit a given metadata's due date
mdEditDue :: Metadata -> Maybe DateTime -> Metadata
mdEditDue md mdd = md { mdDue = mdd }

instance Binary Metadata where
   get = do
      mmdnts <- get :: Get (Maybe BW.ByteString)
      mdp    <- get :: Get Priority
      mdc    <- get :: Get Bool
      mmdd   <- get :: Get (Maybe DateTime)
      return Metadata { mdNotes = mmdnts
                      , mdPriority = mdp
                      , mdCompleted = mdc
                      , mdDue = mmdd }

   put md =
      put ( mdNotes md
          , mdPriority md
          , mdCompleted md
          , mdDue md )

-- | The metadata helper, taking the following optional parameters:
-- a string for the object's notes, priority for its priority, a boolean
-- representing object completion status, and a datetime, representing
-- a due-date.
metadata :: Maybe String -> Maybe Priority -> Maybe Bool -> Maybe DateTime -> Metadata
metadata mnts mp mc md =
   Metadata { mdNotes = fmap bs mnts
            , mdPriority = fromMaybe Low mp
            , mdCompleted = fromMaybe False mc
            , mdDue = md }
