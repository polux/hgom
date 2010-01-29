{- 

Turns input into a list of escaped strings.
Usage:

brauner@worf:~/hgom/devutil$ cat > text
public void f() {
  String x = "aaa\n";
}
brauner@worf:~/hgom/devutil$ runhaskell tostrlist.hs < text 
["public void f() {",
 "  String x = \"aaa\\n\";",
 "}"]
brauner@worf:~/hgom/devutil$ 

-}

import Data.List(intercalate)

transform = pack . intercalate ",\n " . map show . lines
  where pack s = "[" ++ s ++ "]\n"

main = interact transform
