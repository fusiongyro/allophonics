module Phone where

{-

Phone.

This module provides the low-level data types and functions for phones. 

-}

import Data.Map as Map
import Data.Set as Set

data IsPresent = Present | Absent deriving (Show, Eq)
data MannerFeature = 
      Consonantal
    | Sonorant 
    | Continuant
    | DelayedRelease
    | Approximant
    | Tap
    | Trill
    | Nasal
    deriving (Show, Eq)

data LaryngealFeature = 
      Voice
    | SpreadGlottis
    | ConstrictedGlottis
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data Feature = 
      M MannerFeature IsPresent
    | L LaryngealFeature IsPresent
    | P PlaceFeature IsPresent
    deriving (Show, Eq)

type Phone = Set.Set Feature


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

type IPAPhoneMap = Map.Map String Phone

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
loadCorpus :: FilePath -> IO [[Phone]]
loadCorpus = undefined

