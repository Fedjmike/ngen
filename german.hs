module German where

import Language

-- Morphology

eInflect = addSuffix "e"
enInflect = addSuffix "en"
erInflect = addSuffix "er"
emInflect = addSuffix "em"
esInflect = addSuffix "es"

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
