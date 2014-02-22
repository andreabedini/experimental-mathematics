#!/usr/bin/env runhaskell
import Text.Pandoc.JSON
import Data.List

doHeader :: Block -> Block
doHeader header@(Header level (id, classes, namevals) name)
	| "exercise" `isPrefixOf` id = RawBlock (Format "latex") "\\exercise"
	| otherwise 				 = header
doHeader x = x

main :: IO ()
main = toJSONFilter doHeader
