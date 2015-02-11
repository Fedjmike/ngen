module Dutch where

import Language

-- Words

girl :: Noun
girl number = let
	word _ = case number of
			S -> "meisje"
			P -> "meisjes"
		: []
	in (word, number, N)

the :: Modifier
the (object, number, gender) = let
    str c = case number of
			S -> case gender of
				N -> "het"
				_ -> "de"
			P -> "de"
		: (object c)
    in (str, number, gender)
	
an :: Modifier
an (object, number, gender) = let
	str c = case number of
			S -> ["een"]
			P -> []
		++ (object c)
	in (str, number, gender)

sleeps :: Verb
sleeps S = ["slaapt"]
sleeps P = ["slaap"]

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