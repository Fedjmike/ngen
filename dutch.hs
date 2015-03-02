module Dutch where

import Language

-- Words

girl = noun "meisje" "meisjes" N

the :: Modifier
the = let
	det _ P _ = "de"
	det _ S N = "het"
	det _ S _ = "de"
	in modifier det

an :: Modifier
an = let
	det _ S _ = ["een"]
	det _ P _ = []
	in modifierE f
			
sleeps = verb "slaapt" "slaap"

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
