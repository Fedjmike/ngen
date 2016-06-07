module German where

import Language

data AdjectiveInflection = Strong | Mixed | Weak deriving Eq

-- Morphology

eInflect = addSuffix "e"
enInflect = addSuffix "en"
erInflect = addSuffix "er"
emInflect = addSuffix "em"
esInflect = addSuffix "es"

--  M      N       F      P
-- male neutral femaleOrPlural
--  en
--     em         er      en
--     es             er
article :: (String, String, String, String, String, String, String) -> Number -> Gender -> Case -> String
article (male, neutral, femaleOrPlural, en, em, es, er) =
    let declined number gender c | c == Dat || c == Gen = case (c, number, gender) of
            (c, S, gender) | gender == M || gender == N -> case c of
                Dat -> em
                Gen -> es
                
            (Dat, P, _) -> en
            _ -> er
        
        -- Nom|Acc
        declined S M Nom = male
        declined S M Acc = en
        declined S N _ = neutral
        declined _ _ _ = femaleOrPlural
        
    in declined

attributiveAdjective :: String -> Number -> Gender -> Case -> AdjectiveInflection -> String
attributiveAdjective stem number gender c inflection = let
    e = eInflect
    er = erInflect
    en = enInflect
    em = emInflect
    es = esInflect

    -- Dat|Gen
    suffix c i number gender | c == Dat || c == Gen = case i of
        Strong -> case (number, gender, c) of
            (P, _, Dat) -> en
            (P, _, Gen) -> er
            (S, F, _) -> er
            (S, _, Dat) -> em
            (S, _, Gen) -> en
        _ -> en
        
    -- Nom|Acc:
    
    suffix _ Strong P _ = e
    suffix _ _      P _ = en
    
    -- Mixed|Strong M|N
    suffix c i S gender | (i == Mixed || i == Strong) && (gender == M || gender == N) = case (c, gender) of
        (Nom, M) -> er
        (Acc, M) -> en
        (_, N) -> es
    
    suffix Acc _ S M = en
    suffix _   _ S _ = e
    
    in (suffix c inflection number gender) stem
   
-- Words

personalPronoun :: Number -> Gender -> Person -> Case -> String
-- Third person pronouns are fairly article-like
personalPronoun number gender ThirdPerson c = case (number, gender, c) of
    (S, F, Dat) -> "ihr"
    (P, _, Dat) -> "ihnen"
    _ -> article ("er", "es", "sie", "ihn", "ihm", "seiner", "ihrer") number gender c

personalPronoun S _ FirstPerson Nom = "ich"
personalPronoun S _ SecondPerson Nom = "du"

-- Singular pronouns follow a pattern of the stem indicating person and a suffix for case
personalPronoun S _ person c =
    let stem FirstPerson = "m"
        stem SecondPerson = "d"
        
    in stem person ++ case c of
        Acc -> "ich"
        Dat -> "ir"
        Gen -> "einer"
    
-- Plural pronouns are not very patternful
personalPronoun P _ person c =
    let pronoun nom obj gen = case c of
            Nom -> nom
            Gen -> gen
            _ -> obj
        
    in case person of
        FirstPerson -> pronoun "wir" "uns" "unser"
        SecondPerson -> pronoun "ihr" "euch" "euer"
        
definiteArticle = article ("der", "das", "die", "den", "dem", "des", "der")
indefiniteArticle = article ("ein", "ein", "eine", "einen", "einem", "eines", "einer")

the = modifier definiteArticle
an = modifier indefiniteArticle

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
    ++ (concat (map acc objects))

question :: Clause
question (subject, number, _) verb objects =
       (verb number)
    ++ (subject Nom)
    ++ (concat (map acc objects))
