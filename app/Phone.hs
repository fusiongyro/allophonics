{-# LANGUAGE OverloadedStrings #-}

module Phone where

{-

Phone.

This module provides the low-level data types and functions for phones. 

-}

{-

Phone.

This module provides the low-level data types and functions for phones. 

-}
import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Map as Map hiding (mapMaybe, toList, (!))
import Data.Set as Set hiding (toList)
import Data.Vector as V hiding (mapMaybe, (!))
import Data.ByteString.Lazy as BS
import Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Csv as CSV
import Data.HashMap.Strict (toList, (!))


data IsPresent = Present | Absent deriving (Show, Eq, Ord)
data MannerFeature =
      Consonantal
    | Sonorant
    | Continuant
    | DelayedRelease
    | Approximant
    | Tap
    | Trill
    | Nasal
    deriving (Show, Eq, Ord)

data LaryngealFeature =
      Voice
    | SpreadGlottis
    | ConstrictedGlottis
    deriving (Show, Eq, Ord)

data PlaceFeature =
      Labial
    | Round
    | Labiodental
    | Coronal
    | Anterior
    | Distributed
    | Strident
    | Lateral
    | Dorsal
    | High
    | Low
    | Front
    | Back
    | Tense
    deriving (Show, Eq, Ord)

data Feature =
      M MannerFeature IsPresent
    | L LaryngealFeature IsPresent
    | P PlaceFeature IsPresent
    deriving (Show, Eq, Ord)

type Phone = Set.Set Feature

-- | presentFromChar is Absent or Present depending on the character
presentFromChar :: (Eq s, IsString s) => s -> IsPresent
presentFromChar "+" = Present
presentFromChar _ = Absent


-- This is a "function" that makes it easy to determine what to do with the CSV's headers
-- Note that these features I don't know what to do with yet: syllabic, stress, long
csvHeaderToFeature :: (IsString s, Ord s) => Map.Map s (IsPresent -> Feature)
csvHeaderToFeature = Map.fromList [
    ("consonantal", M Consonantal)
  , ("sonorant", M Sonorant)
  , ("continuant", M Continuant)
  , ("delayed release", M DelayedRelease)
  , ("approximant", M Approximant)
  , ("tap", M Tap)
  , ("trill", M Trill)
  , ("nasal", M Nasal)
  , ("voice", L Voice)
  , ("spread gl", L SpreadGlottis)
  , ("constr gl", L ConstrictedGlottis)
  , ("LABIAL", P Labial)
  , ("round", P Round)
  , ("labiodental", P Labiodental)
  , ("CORONAL", P Coronal)
  , ("anterior", P Anterior)
  , ("distributed", P Distributed)
  , ("strident", P Strident)
  , ("lateral", P Lateral)
  , ("DORSAL", P Dorsal)
  , ("high", P High)
  , ("low", P Low)
  , ("front", P Front)
  , ("back", P Back)
  , ("tense", P Tense)]

recordToFeature :: (IsString s, Eq s, Ord s) => (s, s) -> Maybe Feature
recordToFeature (_, "0") = Nothing
recordToFeature (s, c) = Map.lookup s csvHeaderToFeature <*> Just (presentFromChar c)

{-

Encode and decode functions. Useful for going between Unicode IPA and Phones

-}

-- | decodePhone decodes a Unicode IPA symbol to a Phone
decodePhone :: String -> Phone
decodePhone = undefined

-- | encodePhone converts a phone into a Unicode IPA symbol
encodePhone :: Phone -> String
encodePhone = undefined

{-

Feature file parsing. The feature file maps IPA symbols to Phones, each of which
is a set of features.

We want this to exist as a map so that we can convert phones back to some kind of
IPA representation. But sometimes there is no IPA symbol for something, we have to
construct one out of a symbol plus some diacritic marks.

The internal representation in our application is Phones. This facility makes it 
possible to decode and encode phonemic representations to phones.

-}

type IPAPhoneMap = Map.Map Text Phone

-- | wordPhones converts a word (in IPA) into a list of Phones
wordPhones :: IPAPhoneMap -> String -> [Phone]
wordPhones = undefined

-- | phonesWord converts a list of Phones back into a word (using IPA)
phonesWord :: IPAPhoneMap -> [Phone] -> String
phonesWord = undefined

-- | loadFeatures reads the CSV file with the phones/features matrix in it
loadFeatures :: FilePath -> IO IPAPhoneMap
loadFeatures fp = do
  -- read the file
  fc <- BS.readFile fp

  -- parse the CSV (gross)
  let Right (header, rec) = CSV.decodeByName fc

  -- piece it together
  return $ Map.fromList $ V.toList $ V.map recordToFeatureList rec

recordToFeatureList :: NamedRecord -> (T.Text, Phone)
recordToFeatureList rec = (symbol, phone)
  where 
    symbol = decodeUtf8 $ rec ! "symbol"
    phone = Set.fromList $ mapMaybe recordToFeature $ Data.HashMap.Strict.toList rec
