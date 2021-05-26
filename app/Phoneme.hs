module Phoneme where

import Phone

{-

Phoneme

This module knows the difference between phones and phonemes.
We present a data type for phonemes which consists of a base
phone, plus a number of context rules for alternate phones. 
The combination of the base phone and the rules for alternates
is itself a conceptual phoneme in the corpus.

-}

data WordContext = WordInitial | WordMedial | WordFinal
data Neighbor = Preceding | Succeeding


data Context = WordBoundary


data Rule = Rule Context Phone

data Phoneme = Phoneme Phone [Rule]


-- | combinePhonemes takes two phonemes and reduces them to one by combining their rules
combinePhonemes :: Phoneme -> Phoneme -> Phoneme
combinePhonemes (Phoneme p1 rules1) (Phoneme p2 rules2) = 
    -- p1 and p2 had better be equal
    undefined 
