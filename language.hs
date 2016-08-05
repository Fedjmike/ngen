module Language where

-- Grammatical states

data Number = S | P deriving (Show, Eq)
data Gender = M | F | N deriving (Show, Eq)
data Person = FirstPerson | SecondPerson | SecondPersonFormal | ThirdPerson deriving (Show, Eq)

-- This typeclass links the cases of different languages
class Case c

-- Typedefs

type NounPhrase c = (c -> [String], Number, Gender)

type Noun c = Number -> NounPhrase c
type Modifier c = NounPhrase c -> NounPhrase c

type Adjective c = Number -> Gender -> c -> [String]

type Verb = Number -> [String]
type Clause c = NounPhrase c -> Verb -> [NounPhrase c] -> [String]

applyCase :: Case c => c -> NounPhrase c -> [String]
applyCase c (object, _, _) = object c

noun :: Case c => String -> String -> Gender -> Noun c
noun single plural gender = 
	\number ->
		(\_ -> case number of
			S -> [single]
			P -> [plural],
		 number, gender)

modifier :: Case c => (Number -> Gender -> c -> String) -> Modifier c
modifier f (object, number, gender) = (\c -> f number gender c : object c, number, gender)

modifierE :: Case c => (Number -> Gender -> c -> [String]) -> Modifier c
modifierE f (object, number, gender) = (\c -> f number gender c ++ object c, number, gender)

verb :: String -> String -> Verb
verb single _ S = [single]
verb _ plural P = [plural]

-- Morphology helpers

addSpecialSuffix :: (String -> Maybe String) -> String -> String -> String
addSpecialSuffix _ standard "" = standard
addSpecialSuffix special standard stem@(c:cs) = case special stem of
    Just suffix -> stem ++ suffix
    Nothing -> c : addSpecialSuffix special standard cs

addSuffix :: String -> String -> String
addSuffix = let
    nullSpecial _ = Nothing
    in addSpecialSuffix nullSpecial
