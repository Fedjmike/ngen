module English where

import Language

-- Morphology

pluralize :: String -> String
-- 'o' gets -es if preceded by a consonant
pluralize "ao" = "aos"
pluralize "eo" = "eos"
pluralize "io" = "ios"
pluralize "oo" = "oos"
pluralize "uo" = "uos"
pluralize "o" = "oes"
-- Sibilants (air passing through teeth) also
-- Missing: "zh" and "dg"
-- (doesn't tend to happen without an -e though, so shouldn't be a problem)
pluralize "s" = "ses"
pluralize "z" = "zes"
pluralize "sh" = "shes"
pluralize "ch" = "ches"
--
pluralize "y" = "ies"
--
pluralize (c:[]) = c : "s"
pluralize (c:cs) = c : pluralize cs

noun_m singular = noun singular (pluralize singular)
verb_m singular = verb (pluralize singular) singular

-- Words

girl = noun_m "girl" F
cat = noun_m "cat" N

the :: Modifier
the (object, number, gender) = (\c -> "the" : (object c), number, gender)
	
an :: Modifier
an (object, number, gender) = let
	str c = case number of
			S -> ["an"]
			P -> []
		++ (object c)
	in (str, number, gender)

sleeps = verb_m "sleep"
eats = verb_m "eat"

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
