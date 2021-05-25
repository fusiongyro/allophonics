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

data Context = WordInitial
             | WordFinal
             | Preceding Phone
             | Succeeding Phone

data Rule = Rule Context Phone

data Phoneme = Phoneme Phone [Rule]
