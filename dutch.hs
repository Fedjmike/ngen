module Dutch where

import Language

data Case = Nom | Acc deriving (Show, Eq)
instance Language.Case Dutch.Case

(nom, acc) = fmap applyCase (Nom, Acc)

-- Words

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
	
girl = noun "meisje" "meisjes" N
cat = noun "kat" "katten" F
			
sleeps = verb "slaapt" "slaap"
eats = verb "eet" "eten"

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
