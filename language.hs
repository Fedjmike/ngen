module Language where

-- Grammatical states

data Number = S | P
data Gender = M | F | N deriving Eq
data Case = Nom | Gen | Dat | Acc deriving Eq
data Person = FirstPerson | SecondPerson | ThirdPerson

-- Typedefs

type NounPhrase = (Case -> [String], Number, Gender)

type Noun = Number -> NounPhrase
type Modifier = NounPhrase -> NounPhrase

type Adjective = Number -> Gender -> Case -> [String]

type Verb = Number -> [String]
type Clause = NounPhrase -> Verb -> [NounPhrase] -> [String]

nom :: NounPhrase -> [String]
gen :: NounPhrase -> [String]
dat :: NounPhrase -> [String]
acc :: NounPhrase -> [String]
nom (object, _, _) = object Nom
gen (object, _, _) = object Gen
dat (object, _, _) = object Dat
acc (object, _, _) = object Acc

noun :: String -> String -> Gender -> Noun
noun single plural gender = 
	\number ->
		(\_ -> case number of
			S -> [single]
			P -> [plural],
		 number, gender)

modifier :: (Case -> Number -> Gender -> String) -> Modifier
modifier f (object, number, gender) = (\c -> (f c number gender) : (object c), number, gender)

modifierE :: (Case -> Number -> Gender -> [String]) -> Modifier
modifierE f (object, number, gender) = (\c -> (f c number gender) ++ (object c), number, gender)

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
