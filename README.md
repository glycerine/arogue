arogue
=======
This is the beginnings of a port of the lovely ([gopherjs (go -> javascript) transpiler](https://github.com/neelance/gopherjs)) to produce an R-based repl and transpiler from Go -> R.

The goal is to take the world's best data analysis and debugging environment (in what other debugging environment can you stop in the middle of your code and histogram your data for outliers?) and bring its power to Go. 

Status: just starting. Run "go test -v" and see bb_test.go (and any other _test.go files that may be added) 
to discern what tests are in place. All tests should pass at all times; this is BDD after all. 

To summarize: string, boolean and numeric constants work. Assignment works, but swapping with parallel assignment does not (e.g. "a, b = b, a" will produce a = b; b = a; probably not what you want). Most binary operators work. Simple function definitions of short functions work. fmt.Printf is handled to provide diagnostic printing. 

NB: Initially we were targeting scheme, but R is a better choice, it turns out. The scheme facilities for inspecting data and plotting just don't compare. We plan to hook the parser up to Rserve shortly, but at the moment it targets a chicken scheme repl.

MIT or Apache2 license.

