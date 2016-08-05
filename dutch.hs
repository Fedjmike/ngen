module Dutch where

import Language

data Case = Nom | Acc deriving (Show, Eq)
instance Language.Case Dutch.Case

(nom, acc) = fmap applyCase (Nom, Acc)

-- Declension

articleLike :: String -> String -> Number -> Gender -> Dutch.Case -> String
articleLike _  het S N _ = het
articleLike de _   S _ _ = de
articleLike de _   P _ _ = de

-- Words

definiteArticle = articleLike "de" "het"
indefiniteArticle _ _ _ = "een"

[the, an] = map modifier [definiteArticle, indefiniteArticle]

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
