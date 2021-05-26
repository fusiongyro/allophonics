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

recordToFeature :: (Eq s, IsString s) => (s, s) -> Maybe Feature
recordToFeature (_, "0") = Nothing
recordToFeature ("consonantal", c) = Just $ M Consonantal $ presentFromChar c
recordToFeature ("sonorant", c) = Just $ M Sonorant $ presentFromChar c
recordToFeature ("continuant", c) = Just $ M Continuant $ presentFromChar c
recordToFeature ("delayed release", c) = Just $ M DelayedRelease $ presentFromChar c
recordToFeature ("approximant", c) = Just $ M Approximant $ presentFromChar c
recordToFeature ("tap", c) = Just $ M Tap $ presentFromChar c
recordToFeature ("trill", c) = Just $ M Trill $ presentFromChar c
recordToFeature ("nasal", c) = Just $ M Nasal $ presentFromChar c

recordToFeature ("voice", c) = Just $ L Voice $ presentFromChar c
recordToFeature ("spread gl", c) = Just $ L SpreadGlottis $ presentFromChar c
recordToFeature ("constr gl", c) = Just $ L ConstrictedGlottis $ presentFromChar c

recordToFeature ("LABIAL", c) = Just $ P Labial $ presentFromChar c
recordToFeature ("round", c) = Just $ P Round $ presentFromChar c
recordToFeature ("labiodental", c) = Just $ P Labiodental $ presentFromChar c
recordToFeature ("CORONAL", c) = Just $ P Coronal $ presentFromChar c
recordToFeature ("anterior", c) = Just $ P Anterior $ presentFromChar c
recordToFeature ("distributed", c) = Just $ P Distributed $ presentFromChar c
recordToFeature ("strident", c) = Just $ P Strident $ presentFromChar c
recordToFeature ("lateral", c) = Just $ P Lateral $ presentFromChar c
recordToFeature ("DORSAL", c) = Just $ P Dorsal $ presentFromChar c
recordToFeature ("high", c) = Just $ P High $ presentFromChar c
recordToFeature ("low", c) = Just $ P Low $ presentFromChar c
recordToFeature ("front", c) = Just $ P Front $ presentFromChar c
recordToFeature ("back", c) = Just $ P Back $ presentFromChar c
recordToFeature ("tense", c) = Just $ P Tense $ presentFromChar c

-- these features I don't know what to do with yet
recordToFeature ("syllabic", c) = Nothing
recordToFeature ("stress", c) = Nothing 
recordToFeature ("long", c) = Nothing 

-- finally, a catchall
recordToFeature _ = Nothing

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

-- | loadFeatures loads a features CSV file into an IPAPhoneMap
loadFeatures :: FilePath -> IO IPAPhoneMap
loadFeatures = undefined

-- | wordPhones converts a word (in IPA) into a list of Phones
wordPhones :: IPAPhoneMap -> String -> [Phone]
wordPhones = undefined

-- | phonesWord converts a list of Phones back into a word (using IPA)
phonesWord :: IPAPhoneMap -> [Phone] -> String
phonesWord = undefined

-- | loadCorpus reads a file with the corpus words in it
loadCorpus :: FilePath -> IO IPAPhoneMap
loadCorpus fp = do
  -- read the file
  fc <- BS.readFile fp

  -- parse the CSV (gross)
  let Right (header, rec) = CSV.decodeByName fc

  -- piece it together
  return $ Map.fromList $ V.toList $ V.map recordToFeatureList rec

recordToFeatureList :: NamedRecord -> (T.Text, Phone)
recordToFeatureList rec = (decodeUtf8 $ rec ! "symbol", Set.fromList $ mapMaybe recordToFeature (Data.HashMap.Strict.toList rec))