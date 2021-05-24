module Phone where

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


-- | decodePhone decodes a Unicode IPA symbol to a Phone
decodePhone :: String -> Phone
decodePhone = undefined 

-- | encodePhone converts a phone into a Unicode IPA symbol
encodePhone :: Phone -> String
encodePhone = undefined
