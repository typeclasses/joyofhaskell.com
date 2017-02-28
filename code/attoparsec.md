```haskell
import Data.Attoparsec.Text

λ> parseOnly (char 'a') "a"
-- Right 'a'

λ> parseOnly (char 'a') ""
-- Left "'a': not enough input"

λ> parseOnly (char 'a') "b"
-- Left "'a': Failed reading: satisfy"

λ> parseOnly
     (many (char 'a') <* char 'b')
     "aaab"
-- Right "aaa"
```
