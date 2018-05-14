#load "AST.fs"
#r "bin/Debug/netcoreapp2.0/publish/Piglet.dll"
#load "PigletWrappers.fs"
#load "Parser.fs"

open Boyarlang.Compiler

Parser.parse "цѣл a;"