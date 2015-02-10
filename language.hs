module Language where

-- Grammatical states

data Number = S | P
data Gender = M | F | N
data Case = Nom | Gen | Dat | Acc

-- Typedefs

type NounPhrase = (Case -> [String], Number, Gender)

type Noun = Number -> NounPhrase
type Modifier = NounPhrase -> NounPhrase

type Adjective = Number -> Gender -> Case -> [String]

type Verb = Number -> [String]
type Clause = NounPhrase -> Verb -> [NounPhrase] -> [String]