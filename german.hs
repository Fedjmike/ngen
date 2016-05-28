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
	det Nom P _ = "die"
	det Acc P _ = "die"
	det Dat P _ = "den"
	det Gen P _ = "der"
	det Nom S M = "der"
	det Acc S M = "den"
	det Dat S M = "dem"
	det Gen S M = "des"
	det Nom S F = "die"
	det Acc S F = "die"
	det Dat S F = "der"
	det Gen S F = "der"
	det Nom S N = "das"
	det Acc S N = "das"
	det Dat S N = "dem"
	det Gen S N = "des"
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
