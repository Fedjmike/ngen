module English where

import Language

-- Words

girl = noun "girl" "girls" F
cat = noun "cat" "cats" N

the :: Modifier
the (object, number, gender) = (\c -> "the" : (object c), number, gender)
	
an :: Modifier
an (object, number, gender) = let
	str c = case number of
			S -> ["an"]
			P -> []
		++ (object c)
	in (str, number, gender)

sleeps = verb "sleeps" "sleep"
eats = verb "eats" "eat"

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
