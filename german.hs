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
article :: (String, String, String, String, String, String, String) -> Case -> Number -> Gender -> String
article (male, neutral, femaleOrPlural, en, em, es, er) =
    let declined c number gender | c == Dat || c == Gen = case (c, number, gender) of
            (c, S, gender) | gender == M || gender == N -> case c of
                Dat -> em
                Gen -> es
                
            (Dat, P, _) -> en
            _ -> er
        
        -- Nom|Acc
        declined Nom S M = male
        declined Acc S M = en
        declined _   S N = neutral
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
