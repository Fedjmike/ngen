module English where

import Language

data Case = Nom | Acc deriving (Show, Eq)
instance Language.Case English.Case

(nom, acc) = fmap applyCase (Nom, Acc)

isVowel c = c `elem` "aeiou"
isConsonant = not . isVowel

---- Morphology ----

pluralize =
    let special "y" = "ies"
        -- 'o' gets -es if preceded by a consonant
        special (c:"o") | isConsonant c = c:"oes"
        -- Sibilants (air passing through teeth) also
        special str | str `elem` ["s", "z", "sh", "ch"] = str ++ "es"
    in addSpecialSuffix "s" $ maybeize special

---- Declension ----

byCase nom _   Nom = nom
byCase _ acc   Acc = acc

noun :: String -> Noun English.Case
noun singular = Language.noun singular (pluralize singular) N

---- Conjugation ----

irregularVerb :: String -> String -> Verb
irregularVerb single _   S = [single]
irregularVerb _ plural   P = [plural]

-- The singular looks like a plural form of the verb
verb stem = irregularVerb (pluralize stem) stem

---- Words ----

the, an :: Modifier English.Case
the = modifier (\_ _ _ -> "the")

an = extendNP $ \number _ _ phrase ->
    let firstLetter = head.head phrase
        determiner = if isVowel firstLetter then "an" else "a"
    in if number == P then phrase else determiner : phrase

personalPronoun :: Person -> Number -> Gender -> English.Case -> String
personalPronoun FirstPerson S _ = byCase "I" "me"
personalPronoun FirstPerson P _ = byCase "we" "us"
personalPronoun SecondPerson _ _ = \c -> "you"
personalPronoun ThirdPerson P _ = \c -> "they"
personalPronoun ThirdPerson _ N = \c -> "it"
personalPronoun ThirdPerson _ M = byCase "he" "him"
personalPronoun ThirdPerson _ F = byCase "she" "her"

girl = English.noun "girl"
cat = English.noun "cat"

sleeps = verb "sleep"
eats = verb "eat"

---- Structures ----

statement :: Clause English.Case
statement (subject, number, _) verb objects =
       (subject Nom)
    ++ (verb number)
    ++ (concatMap acc objects)

question :: Clause English.Case
question (subject, number, _) verb objects =
       (verb number)
    ++ (subject Nom)
    ++ (concatMap acc objects)
