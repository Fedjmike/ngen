module German where

import Language

data AdjectiveInflection = Strong | Mixed | Weak deriving Eq

-- Morphology

eInflect = addSuffix "e"
enInflect = addSuffix "en"
erInflect = addSuffix "er"
emInflect = addSuffix "em"
esInflect = addSuffix "es"

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

the :: Modifier
the = let
    det c number gender | c == Nom || c == Acc = case (c, number, gender) of
        (Nom, S, M) -> "der"
        (Acc, S, M) -> "den"
        (_, S, N) -> "das"
        _ -> "die"
        
    det c _ gender | gender == M || gender == N = case c of
        Dat -> "dem"
        Acc -> "des"
        
    det Dat P _ = "den"
    det _ _ _ = "der"
    
    in modifier det

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
