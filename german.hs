module German where

import Language

-- Words

cat = noun "katze" "katzen" F
girl = noun "mädchen" "mädchen" N

-- capitalize nouns

the :: Modifier
the (object, number, gender) = let
    detf c = case number of
        S -> case gender of
            M -> case c of
                Nom -> "der"
                Gen -> "des"
                Dat -> "dem"
                Acc -> "den"
            F -> case c of
                Nom -> "die"
                Gen -> "der"
                Dat -> "der"
                Acc -> "die"
            N -> case c of
                Nom -> "das"
                Gen -> "des"
                Dat -> "dem"
                Acc -> "das"
        P -> case c of
            Nom -> "die"
            Gen -> "der"
            Dat -> "den"
            Acc -> "die"
    in (\c -> (detf c) : (object c), number, gender)

sleeps = verb "schläft" "schlafen"
eats = verb "isst" "essen"

-- Structures

statement :: Clause
statement (subject, number, gender) verb objects =
       (subject Nom)
    ++ (verb number)
    ++ (concat (map acc objects))

question :: Clause
question (subject, number, gender) verb objects =
       (verb number)
    ++ (subject Nom)
    ++ (concat (map acc objects))
