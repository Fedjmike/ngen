module German where

import Data.Char
import Language

data Case = Nom | Acc | Dat | Gen deriving (Show, Eq)
instance Language.Case German.Case

data AdjectiveInflection = Strong | Mixed | Weak deriving Eq

nom, acc, dat, gen :: NounPhrase German.Case -> [String]
[nom, acc, dat, gen] = map applyCase [Nom, Acc, Dat, Gen]

byCase nom _ _ _   Nom = nom
byCase _ acc _ _   Acc = acc
byCase _ _ dat _   Dat = dat
byCase _ _ _ gen   Gen = gen

-- Morphology

eInflect = addSuffix "e"
stInflect = addSuffix "st"
tInflect = addSuffix "t"
enInflect = addSuffix "en"
erInflect = addSuffix "er"
emInflect = addSuffix "em"
esInflect = addSuffix "es"

capitalize [] = []
capitalize (c:cs) = toUpper c : cs

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
attributiveAdjective stem number gender c inflection = let
    e = eInflect
    er = erInflect
    en = enInflect
    em = emInflect
    es = esInflect

    -- Dat|Gen
    suffix number gender c i | c == Dat || c == Gen = case i of
        -- In the case of no article, the adjective itself resembles an article
        Strong -> case (number, gender, c) of
            (S, F, _) -> er
            (S, _, Dat) -> em
            (S, _, Gen) -> en
            (P, _, Dat) -> en
            (P, _, Gen) -> er
        _ -> en
        
    -- Nom|Acc:
    
    suffix P _ _ Strong = e
    suffix P _ _ _ = en
    
    -- Mixed|Strong M|N
    -- Resembles the same corner in article declension (der/den + das)
    -- Provides gender and case information that would otherwise come from the definite article
    suffix S gender c i | (i == Mixed || i == Strong) && (gender == M || gender == N) = case (gender, c) of
        (M, Nom) -> er
        (M, Acc) -> en
        (N, _) -> es
    
    suffix S M Acc _ = en
    suffix S _ _ _ = e
    
    in (suffix number gender c inflection) stem
   
-- Words

definiteArticle = article ("der", "das", "die", "den", "dem", "des", "der")
indefiniteArticle = article ("ein", "ein", "eine", "einen", "einem", "eines", "einer")

the = modifier definiteArticle
an = modifier indefiniteArticle

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

cat = noun "katze" "katzen" F
girl = noun "mädchen" "mädchen" N
-- TODO: capitalize nouns

sleeps = verb "schläft" "schlafen"
eats = verb "isst" "essen"

-- Structures

statement :: Clause
statement (subject, number, _) verb objects =
       (subject Nom)
    ++ (verb number)
    ++ (concatMap acc objects)

question :: Clause
question (subject, number, _) verb objects =
       (verb number)
    ++ (subject Nom)
    ++ (concatMap acc objects)
