﻿import Data.List

import Language
import German

-- Helpers

write :: [String] -> IO ()
write sentence = print (intercalate " " sentence)

--

sentences = [statement (the (girl P)) sleeps [],
             statement (the (girl S)) eats [the (cat S)],
             statement (the (cat P)) sleeps []]
		 
main :: IO ()
main = mapM_ write sentences