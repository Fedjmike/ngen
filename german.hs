module German where

import Data.Char
import Language

data Case = Nom | Acc | Dat | Gen deriving (Show, Eq)
instance Language.Case German.Case

data AdjectiveInflection = Strong | Mixed | Weak deriving Eq

nom, acc, dat, gen :: NounPhrase German.Case -> [String]
[nom, acc, dat, gen] = map applyCase [Nom, Acc, Dat, Gen]

---- Morphology ----

eInflect = addSuffix "e"
erInflect = addSuffix "er"
emInflect = addSuffix "em"
esInflect = addSuffix "es"

enInflect =
    let special str | str `elem` ["e", "el", "er"] = str ++ "n"
        special "en" = "en"
    in addSpecialSuffix "en" $ maybeize special

stInflect = 
    let special c | c `elem` ["s", "ß", "z"] = c ++ "t"
        special "t" = "test"
    in addSpecialSuffix "st" $ maybeize special

tInflect = 
    let special "t" = "tet"
    in addSpecialSuffix "t" $ maybeize special

teInflect = eInflect . tInflect

capitalize [] = []
capitalize (c:cs) = toUpper c : cs

---- Declension ----

byCase nom _ _ _   Nom = nom
byCase _ acc _ _   Acc = acc
byCase _ _ dat _   Dat = dat
byCase _ _ _ gen   Gen = gen

--  M      N       F      P
-- male neutral femaleOrPlural
--  en
--     em         er      en
--     es             er
article :: (String, String, String, String, String, String, String) -> Number -> Gender -> German.Case -> String
article (male, neutral, femaleOrPlural, en, em, es, er) =
    let -- Dat|Gen
        declined number gender c | c == Dat || c == Gen = case (number, gender, c) of
            (S, F, _) -> er
            (S, _, Dat) -> em
            (S, _, Gen) -> es
            (P, _, Dat) -> en
            (P, _, Gen) -> er
        
        -- Nom|Acc
        declined S M Nom = male
        declined S M Acc = en
        declined S N _ = neutral
        declined _ _ _ = femaleOrPlural
        
    in declined

attributiveAdjective :: String -> Number -> Gender -> German.Case -> AdjectiveInflection -> String
attributiveAdjective stem =
    let (e, er, en, em, es) = (eInflect stem, erInflect stem, enInflect stem, emInflect stem, esInflect stem)
    
        -- Dat|Gen
        declined number gender c i | c == Dat || c == Gen = case i of
            -- In the case of no article, the adjective itself resembles an article
            Strong -> case (number, gender, c) of
                (S, F, _) -> er
                (S, _, Dat) -> em
                (S, _, Gen) -> en
                (P, _, Dat) -> en
                (P, _, Gen) -> er
            _ -> en
            
        -- Nom|Acc:
        
        declined P _ _ Strong = e
        declined P _ _ _ = en
        
        -- Mixed|Strong M|N
        -- Resembles the same corner in article declension (der/den + das)
        -- Provides gender and case information that would otherwise come from the definite article
        declined S gender c i | (i == Mixed || i == Strong) && (gender == M || gender == N) = case (gender, c) of
            (M, Nom) -> er
            (M, Acc) -> en
            (N, _) -> es
        
        declined S M Acc _ = en
        declined S _ _ _ = e
        
    in declined

---- Words ----

definiteArticle = article ("der", "das", "die", "den", "dem", "des", "der")
indefiniteArticle = article ("ein", "ein", "eine", "einen", "einem", "eines", "einer")
demonstrativeArticle = article ("dieser", "dieses", "diese", "diesen", "diesem", "dieses", "dieser")

[the, an, this] = map modifier [definiteArticle, indefiniteArticle, demonstrativeArticle]

-- "dies-" is both a pronoun and an article
thisPronoun gender number = (\c -> [demonstrativeArticle number gender c], number, gender, ThirdPerson)

personalPronoun :: Person -> Number -> Gender -> German.Case -> String
-- Third person pronouns are fairly article-like
personalPronoun ThirdPerson number gender c = case (number, gender, c) of
    (S, F, Dat) -> "ihr"
    (P, _, Dat) -> "ihnen"
    _ -> article ("er", "es", "sie", "ihn", "ihm", "seiner", "ihrer") number gender c

-- The formal pronoun is the third person plural, capitalised
personalPronoun SecondPersonFormal _ _ c =
    capitalize $ personalPronoun ThirdPerson P N c

personalPronoun FirstPerson  S _ Nom = "ich"
personalPronoun SecondPerson S _ Nom = "du"

-- Singular pronouns follow a pattern of the stem indicating person and a suffix for case
personalPronoun person S _ c =
    case person of
        FirstPerson -> "m"
        SecondPerson -> "d"
    ++ case c of
        Acc -> "ich"
        Dat -> "ir"
        Gen -> "einer"
    
-- Plural pronouns are not very patternful
personalPronoun FirstPerson  P _ c = byCase "wir" "uns" "uns" "unser" c
personalPronoun SecondPerson P _ c = byCase "ihr" "euch" "euch" "euer" c

reflexivePronoun :: Person -> Number -> Gender -> String
reflexivePronoun ThirdPerson _ _ = "sich"
reflexivePronoun p n g = personalPronoun p n g Acc

-- Interrogative pronouns --

( what,  why,     how,   when,   where_) =
 ("was", "warum", "wie", "wann", "wo")

( whereTo, whereFrom) =
 ("wohin", "woher")

who = byCase "wer" "wen" "wem" "wessen"
which = article ("welcher", "welches", "welche", "welchen", "welchem", "welches", "welcher")

---- Vocabulary ----

cat = noun "katze" "katzen" F
girl = noun "mädchen" "mädchen" N
-- TODO: capitalize nouns

---- Verb conjugation ----

-- Simple verbs are conjugated for person/number
-- The most irregular have five forms   e.g. bin bist ist sind seid
simpleVerb first second third plural secondPlural   number person = case (number, person) of    
    (S, FirstPerson) -> [first]
    (S, SecondPerson) -> [second]
    (P, SecondPerson) -> [secondPlural]
    (S, ThirdPerson) -> [third]
    (P, _) -> [plural]
    (_, SecondPersonFormal) -> [plural]

-- Verbs are conjugated with -e, -en, -st, or -t and may have a stem change
verbWithStemChange firstOrPlural secondOrThirdSingle = 
    simpleVerb (eInflect firstOrPlural)
               (stInflect secondOrThirdSingle) (tInflect secondOrThirdSingle)
               (enInflect firstOrPlural) (tInflect firstOrPlural)

-- Strong verbs have a change of stem
strongVerb firstOrPlural secondOrThirdSingle =
    verbWithStemChange firstOrPlural secondOrThirdSingle

-- Weak verbs are completely regular, all forms are based on one stem
weakVerb stem = verbWithStemChange stem stem

---- Words ----

is = simpleVerb "bin" "bist" "ist" "sind" "seid"
has = strongVerb "hab" "ha"
sleeps = strongVerb "schlaf" "schläf"
eats = strongVerb "ess" "iss"
comes = strongVerb "komm" "komm"
knows = simpleVerb "weiß" "weißt" "weiß" "wissen" "wisst"
says = weakVerb "sag"

---- Structures ----

statement :: Clause German.Case
statement (subject, number, _, person) verb objects =
       (subject Nom)
    ++ (verb number person)
    ++ (concatMap acc objects)

question :: Clause German.Case
question (subject, number, _, person) verb objects =
    -- Verb first
       (verb number person)
    ++ (subject Nom)
    ++ (concatMap acc objects)
