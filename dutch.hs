module Dutch where

import Language

data Case = Nom | Acc deriving (Show, Eq)
instance Language.Case Dutch.Case

data Stress = Stressed | Unstressed

(nom, acc) = fmap applyCase (Nom, Acc)

---- Declension ----

byCase nom _   Nom = nom
byCase _ acc   Acc = acc

byStress str _     Stressed = str
byStress _ unstr   Unstressed = unstr

articleLike :: String -> String -> Number -> Gender -> Dutch.Case -> String
articleLike _ het   S N _ = het
articleLike de _    _ _ _ = de

---- Words ----

definiteArticle = articleLike "de" "het"
indefiniteArticle _ _ _ = "een"

[the, an] = map modifier [definiteArticle, indefiniteArticle]

personalPronoun :: Person -> Number -> Gender -> Stress -> Dutch.Case -> String
personalPronoun FirstPerson number _ = flip $ case number of
    S -> byCase (byStress "ik" "'k")
                (byStress "mij" "me")
    P -> byCase (byStress "wij" "we")
                (\_ -> "ons")

personalPronoun SecondPerson number _ = \stress c -> case (number, stress) of
    (S, Stressed) -> byCase "jij" "jou" c
    (P, Stressed) -> "jullie"
    (_, Unstressed) -> "je"
    
personalPronoun SecondPersonFormal _ _ = \stress c -> "u"
    
personalPronoun ThirdPerson number gender = flip $ \c -> case (number, gender, c) of
    (S, M, Nom) -> byStress "hij" "ie"
    (S, M, Acc) -> byStress "hem" "'m"
    -- N doesn't vary by case
    (S, N, _  ) -> byStress "het" "'t"
    -- F|P share the nominative
    (_, _, Nom) -> byStress "zij" "ze"
    (S, F, Acc) -> byStress "haar" "'r"
    (P, _, Acc) -> byStress "hen" "ze"

( who,   whose,   what,  why,      how,   when,      where_) =
 ("wie", "wiens", "wat", "waroom", "hoe", "wanneer", "waar")

which = articleLike "welk" "welke"

girl = noun "meisje" "meisjes" N
cat = noun "kat" "katten" F

---- Verb conjugation ----

simpleVerb first _ _    S FirstPerson = [first]
simpleVerb _ single _   S _ = [single]
simpleVerb _ _ plural   P _ = [plural]

verb single plural = simpleVerb plural single plural

---- Words ----

sleeps = verb "slaapt" "slaap"
eats = verb "eet" "eten"

has = simpleVerb "heb" "heeft" "hebben"

---- Structures ----

statement :: Clause Dutch.Case
statement (subject, number, _, person) verb objects =
       (subject Nom)
    ++ (verb number person)
    ++ (concatMap acc objects)

question :: Clause Dutch.Case
question (subject, number, _, person) verb objects =
       (verb number person)
    ++ (subject Nom)
    ++ (concatMap acc objects)
