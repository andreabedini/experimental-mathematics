#!/usr/bin/env runhaskell
import Text.Pandoc.JSON

doCodeBlocks :: Maybe Format -> Block -> Block
doCodeBlocks (Just (Format "latex")) cb@(CodeBlock (id, classes, namevals) contents)
	| "Mathematica" `elem` classes	= RawBlock (Format "latex")
		$ "\\begin{lstlisting}[language=Mathematica]\n" ++ contents ++ "\n\\end{lstlisting}\n"
	| otherwise 					= cb
doCodeBlocks _ x = x

main :: IO ()
main = toJSONFilter doCodeBlocks
