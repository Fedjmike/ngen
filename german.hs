module German where

import Language

-- Words

cat = noun "katze" "katzen" F
girl = noun "mädchen" "mädchen" N

-- capitalize nouns

the :: Modifier
the = let
	det Nom P _ = "die"
	det Gen P _ = "der"
	det Dat P _ = "den"
	det Acc P _ = "die"
	det Nom S M = "der"
	det Gen S M = "des"
	det Dat S M = "dem"
	det Acc S M = "den"
	det Nom S F = "die"
	det Gen S F = "der"
	det Dat S F = "der"
	det Acc S F = "die"
	det Nom S N = "das"
	det Gen S N = "des"
	det Dat S N = "dem"
	det Acc S N = "das"
    in modifier det

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
