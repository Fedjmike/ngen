module German where

import Language

-- Words

cat :: Noun
cat number = let
    str _ = case number of
        S -> ["Katze"]
        P -> ["Katzen"]
    in (str, number, F)
    
girl :: Noun
girl number = (\_ -> ["Mädchen"], number, N)

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

sleeps :: Verb
sleeps S = ["schläft"]
sleeps P = ["schlafen"]

eats :: Verb
eats S = ["isst"]
eats P = ["essen"]

-- Structures

statement :: Clause
statement (subject, number, gender) verb objects =
       (subject Nom)
    ++ (verb number)
    ++ (concat (map (\(object, _, _) -> object Acc) objects))

question :: Clause
question (subject, number, gender) verb objects =
       (verb number)
    ++ (subject Nom)
    ++ (concat (map (\(object, _, _) -> object Acc) objects))