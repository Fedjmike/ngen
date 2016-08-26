module Language where

import Control.Spoon -- spoon

---- Grammatical states ----

data Number = S | P deriving (Show, Eq)
data Gender = M | F | N deriving (Show, Eq)
data Person = FirstPerson | SecondPerson | SecondPersonFormal | ThirdPerson deriving (Show, Eq)

-- This typeclass links the cases of different languages
class Case c

---- Typedefs ----

type NounPhrase c = (c -> [String], Number, Gender, Person)

type Noun c = Number -> NounPhrase c
type Modifier c = NounPhrase c -> NounPhrase c

type Adjective c = Number -> Gender -> c -> [String]

type Verb = Number -> Person -> [String]
type Clause c = NounPhrase c -> Verb -> [NounPhrase c] -> [String]

applyCase :: Case c => c -> NounPhrase c -> [String]
applyCase c (object, _, _, _) = object c

noun :: Case c => String -> String -> Gender -> Noun c
noun single plural gender = 
	\number ->
		(\_ -> case number of
			S -> [single]
			P -> [plural],
		 number, gender, ThirdPerson)

-- Extend a NounPhrase by providing a function that maps between the old and new sentence fragment
extendNP :: Case c => (Number -> Gender -> c -> [String] -> [String]) -> Modifier c
extendNP f (object, n, g, p) = (\c -> f n g c $ object c, n, g, p)

-- A simpler form for functions that just give a word to be added before the fragment
modifier :: Case c => (Number -> Gender -> c -> String) -> Modifier c
modifier f = extendNP (\n g c o -> f n g c : o)

---- Morphology helpers ----

addSpecialSuffix :: String -> (String -> Maybe String) -> String -> String
addSpecialSuffix standard _ "" = standard
addSpecialSuffix standard special stem@(c:cs) = case special stem of
    Just suffix -> suffix
    Nothing -> c : addSpecialSuffix standard special cs

addSuffix :: String -> String -> String
addSuffix suffix = addSpecialSuffix suffix (\_ -> Nothing)

maybeize f = \x -> spoon $ f x

---- ----

write phrase = print (intercalate " " phrase)
