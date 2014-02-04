arogue
=======

This is the beginnings of a port of the lovely ([gopherjs (go -> javascript) transpiler](https://github.com/neelance/gopherjs)) to produce an R-based repl and transpiler from Go -> R.

The goal is to take the world's best data analysis and debugging environment (in what other debugging environment can you stop in the middle of your code and histogram your data for outliers?) and bring its power to Go. 

Status: just starting. Run "go test -v" and see bb_test.go (and any other _test.go files that may be added) 
to discern what tests are in place. All tests should pass at all times; this is BDD after all. 

To summarize: string, boolean and numeric constants work. Assignment works, but swapping with parallel assignment does not (e.g. "a, b = b, a" will produce a = b; b = a; probably not what you want). Most binary operators work. Simple function definitions of short functions work. fmt.Printf is handled to provide diagnostic printing. 

NB: Initially we were targeting scheme, but R is a better choice, it turns out. The scheme facilities for inspecting data and plotting just don't compare. We plan to hook the parser/translator up to Rserve shortly. Presently it displays the translation without evaluating it.

About the name arogue?  It just sounds cool. And backwards it stands for Environment Using Go Over R by Aten.

MIT or Apache2 license.

sample session
--------------

~~~
go build
./arogue
Welcome the arogue repl! press ctrl-d to exit.
0001 bb> a:=10
line is: 'a:=10'
tranlation to R is: []string{"a = 10;"}
0002 bb> b:=12
line is: 'b:=12'
tranlation to R is: []string{"b = 12;"}
0003 bb> a + b
line is: 'a + b'
tranlation to R is: []string{"a + b"}
0004 bb> func f(a, b int) int { return a * b }
line is: 'func f(a, b int) int { return a * b }'
tranlation to R is: []string{"f = function(a, b) { return(a * b) }"}
0005 bb> fmt.Printf("my var a is: %v\n", a)
line is: 'fmt.Printf("my var a is: %v\n", a)'
tranlation to R is: []string{`cat(paste(sep="", "my var a is: ", a, "\n"))`}
0006 bb> 
~~~

