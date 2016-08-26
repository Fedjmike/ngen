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

---- Words ----

the, an :: Modifier English.Case
the = modifier (\_ _ _ -> "the")

an = extendNP $ \number _ _ phrase ->
    let firstLetter = head $ head phrase
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

[i, we, you, he, it, she, they] =
    [(\c -> [personalPronoun p n g c], n, g, p)
     | (p, n, g) <-    [(FirstPerson, n, N) | n <- [S, P]]
                    ++ [(SecondPerson, S, N)]
                    ++ [(ThirdPerson, n, g) | (n, g) <- [(S, M), (S, N),
                                                         (S, F), (P, N)]]]

girl = English.noun "girl"
cat = English.noun "cat"

---- Verb conjugation ----

simpleVerb singleFirstPerson _ _   S FirstPerson = [singleFirstPerson]
simpleVerb _ singleThirdPerson _   S ThirdPerson = [singleThirdPerson]
simpleVerb _ _ secondOrPlural      _ _ = [secondOrPlural]

-- The single third person looks like a plural form of the verb
verb stem = simpleVerb stem (pluralize stem) stem

---- Words ----

sleeps = verb "sleep"
eats = verb "eat"

is = simpleVerb "am" "is" "are"
has = simpleVerb "have" "has" "have"

---- Structures ----

statement :: Clause English.Case
statement (subject, number, _, person) verb objects =
       (subject Nom)
    ++ (verb number person)
    ++ (concatMap acc objects)

question :: Clause English.Case
question (subject, number, _, person) verb objects =
       (verb number person)
    ++ (subject Nom)
    ++ (concatMap acc objects)
