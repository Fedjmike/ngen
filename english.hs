module English where

import Language

data Case = Nom | Acc deriving (Show, Eq)
instance Language.Case English.Case

(nom, acc) = fmap applyCase (Nom, Acc)

-- Declension

byCase nom _   Nom = nom
byCase _ acc   Acc = acc

-- Morphology

pluralize :: String -> String
-- 'o' gets -es if preceded by a consonant
pluralize "ao" = "aos"
pluralize "eo" = "eos"
pluralize "io" = "ios"
pluralize "oo" = "oos"
pluralize "uo" = "uos"
pluralize "o" = "oes"
-- Sibilants (air passing through teeth) also
-- Missing: "zh" and "dg"
-- (doesn't tend to happen without an -e though, so shouldn't be a problem)
pluralize "s" = "ses"
pluralize "z" = "zes"
pluralize "sh" = "shes"
pluralize "ch" = "ches"
--
pluralize "y" = "ies"
--
pluralize (c:[]) = c : "s"
pluralize (c:cs) = c : pluralize cs

noun_m :: String -> Gender -> Noun English.Case
noun_m singular = noun singular (pluralize singular)
verb_m singular = verb (pluralize singular) singular

-- Words

the :: Modifier English.Case
the = modifier (\_ _ _ -> "the")
	
an :: Modifier English.Case
an orig @ (_, P, _) = orig
an (object, number, gender) = let
	newObject c = let
		nounPhrase = object c
		startOf ((c:_):_) = c
		det 'a' = "an"
		det 'e' = "an"
		det 'i' = "an"
		det 'o' = "an"
		det 'u' = "an"
		det _ = "a"
		in  (det $ startOf nounPhrase) : nounPhrase
	in (newObject, number, gender)

personalPronoun :: Person -> Number -> Gender -> English.Case -> String
personalPronoun FirstPerson S _ = byCase "I" "me"
personalPronoun FirstPerson P _ = byCase "we" "us"
personalPronoun SecondPerson _ _ = \c -> "you"
personalPronoun ThirdPerson P _ = \c -> "they"
personalPronoun ThirdPerson _ N = \c -> "it"
personalPronoun ThirdPerson _ M = byCase "he" "him"
personalPronoun ThirdPerson _ F = byCase "she" "her"

girl = noun_m "girl" F
cat = noun_m "cat" N

sleeps = verb_m "sleep"
eats = verb_m "eat"

-- Structures

statement :: Clause English.Case
statement (subject, number, _) verb objects =
       (subject Nom)
    ++ (verb number)
    ++ (concat (map acc objects))

question :: Clause English.Case
question (subject, number, _) verb objects =
       (verb number)
    ++ (subject Nom)
    ++ (concat (map acc objects))
