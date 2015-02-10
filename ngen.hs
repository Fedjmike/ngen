import Data.List

-- Grammatical states

data Number = S | P
data Gender = M | F | N
data Case = Nom | Gen | Dat | Acc

-- Typedefs

type Object = (Case -> [String], Number, Gender)
type Clause = [String]

type Noun = Number -> Object
type Determiner = Object -> Object
type Verb = Number -> [String]
type VerbPhrase = Object -> Verb -> [Object] -> Clause

-- Words

cat :: Noun
cat number = let
    str _ = case number of
        S -> ["Katze"]
        P -> ["Katzen"]
    in (str, number, F)

the :: Determiner
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
sleeps S = ["schlaeft"]
sleeps P = ["schlafen"]

eats :: Verb
eats S = ["isst"]
eats P = ["essen"]

-- Structures

statement :: VerbPhrase
statement (subject, number, gender) verb objects =
       (subject Nom)
    ++ (verb number)
    ++ (concat (map (\(object, _, _) -> object Acc) objects))

-- Helpers

write :: [String] -> IO ()
write sentence = print (intercalate " " sentence)

--

main :: IO ()
main = do write (statement (the (cat S)) sleeps [])
          write (statement (the (cat S)) eats [the (cat P)])