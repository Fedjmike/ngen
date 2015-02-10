import Data.List

import Language
import German

-- Helpers

write :: [String] -> IO ()
write sentence = print (intercalate " " sentence)

--

main :: IO ()
main = do write (statement (the (cat P)) sleeps [])
          write (statement (the (girl S)) eats [the (cat P)])
          write (question (the (girl P)) sleeps [])