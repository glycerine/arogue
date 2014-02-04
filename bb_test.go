package main

import (
	"strings"
	"testing"

	cv "github.com/smartystreets/goconvey/convey"
)

// testing helper that panics on bad out
func toScheme(line string) string {
	anew := NewAccum()
	s, err := TranslateToScheme(line, anew)
	if err != nil {
		panic(err)
	}
	return strings.Join(s, "")
}

/*
func TestCanPrint(t *testing.T) {

	output := ""
	cv.Convey("Given a birdbrain repl", t, func() {

		cv.Convey("When we see a request to print 'hello bb'", func() {

			cv.Convey("then 'hello bb' should be printed", func() {
				cv.So(output, cv.ShouldEqual, "hello bb")
			})
		})
	})
}
*/

func TestConstantExpressions(t *testing.T) {

	cv.Convey("Given a birdbrain repl", t, func() {
		cv.Convey("When we evaluate numeric constants", func() {
			cv.Convey("they should stay the same", func() {
				cv.So(toScheme("1"), cv.ShouldEqual, "1")
				cv.So(toScheme("23456"), cv.ShouldEqual, "23456")
				cv.So(toScheme("1e40"), cv.ShouldEqual, "1e40")
				cv.So(toScheme("98764.321e12"), cv.ShouldEqual, "98764.321e12")
			})
		})
		cv.Convey("When we evaluate string constants", func() {
			cv.Convey("they should stay the same", func() {
				cv.So(toScheme("`abc`"), cv.ShouldEqual, `"abc"`)
				cv.So(toScheme(`"hello"`), cv.ShouldEqual, `"hello"`)
				cv.So(toScheme(`"I have spaces"`), cv.ShouldEqual, `"I have spaces"`)
				cv.So(toScheme(`"I have \"double\" quotes"`), cv.ShouldEqual, `"I have \"double\" quotes"`)
				cv.So(toScheme("`I have \nnewline`"), cv.ShouldEqual, `"I have \nnewline"`)
			})
			cv.Convey("raw strings should not have their back-quotes sent to scheme (since the back-quote ` is used for template defintion in scheme)", func() {
				cv.So(toScheme("a := `abc he\nya\"`"), cv.ShouldEqual, `a = "abc he\nya\"";`)
			})

		})

		cv.Convey("When we evaluate //scm: comments", func() {
			cv.Convey("they should turn into pass-through scheme code", func() {
				cv.So(toScheme(`//scm:(write "hello")`), cv.ShouldEqual, `(write "hello")`)
				cv.So(toScheme(`// just a comment`), cv.ShouldEqual, `;;// just a comment`)

			})
		})

		cv.Convey("When we evaluate boolean literals", func() {
			cv.Convey("true in golang should become TRUE in R.", func() {
				cv.So(toScheme("true"), cv.ShouldEqual, "TRUE")
			})
			cv.Convey("false in golang should become FALSE in R.", func() {
				cv.So(toScheme("false"), cv.ShouldEqual, "FALSE")
			})
		})
	})
}

func TestIntegerVariables(t *testing.T) {

	cv.Convey("Given a birdbrain repl", t, func() {
		cv.Convey("When we declare and assign an integer variable", func() {
			cv.Convey("then we should get a scheme define expression in return. ", func() {

				cv.So(toScheme("a := 23"), cv.ShouldEqual, "a = 23;")
				cv.So(toScheme(`str := "twentythree"`), cv.ShouldEqual, `str = "twentythree";`)
			})
		})
		cv.Convey("When we just ask for the name of a variable", func() {
			cv.Convey("then we should get a request for the value of that variable. ", func() {
				cv.So(toScheme("a"), cv.ShouldEqual, "a")
				cv.So(toScheme("myVar"), cv.ShouldEqual, "myVar")
			})
		})

		cv.Convey("When we assign more than one variable in parallel in the same := stmt, all should be assigned.", func() {
			cv.So(toScheme("a, b := 10, 12"), cv.ShouldEqual, "a = 10;b = 12;")
		})
	})
}

func TestBinop(t *testing.T) {

	cv.Convey("Given a birdbrain repl", t, func() {
		cv.Convey("When we use addition as in: a + b", func() {
			cv.Convey("then we should get the infix notation a + b", func() {
				cv.So(toScheme("2 + 5"), cv.ShouldEqual, "2 + 5")
			})
		})
		cv.Convey("When we use binary-operations in general", func() {
			cv.Convey("then we should get the infix notation.", func() {
				cv.So(toScheme("2 * 5"), cv.ShouldEqual, "2 * 5")
				cv.So(toScheme("2 / 5"), cv.ShouldEqual, "quotient(2, 5)")
				cv.So(toScheme("2 - 5"), cv.ShouldEqual, "2 - 5")
				cv.So(toScheme("5 % 2"), cv.ShouldEqual, "remainder(5, 2)")

				// in prelude, require(bitops);  and:
				// define quotient  = function(a,b) { if (a*b < 0) { -(abs(a) %/% abs(b)) } else { a %/% b } }
				// define remainder = function(a,b) { if (a*b < 0) { -(abs(a) %% abs(b)) } else { a %% b } }
				cv.So(toScheme("5 / 2"), cv.ShouldEqual, "quotient(5, 2)") // integer division
				cv.So(toScheme("1 << 3"), cv.ShouldEqual, "bitShiftL(1, 3)")
				cv.So(toScheme("32 >> 3"), cv.ShouldEqual, "bitShiftR(32, 3)")
				cv.So(toScheme("32 == 3"), cv.ShouldEqual, "32 == 3")

				cv.So(toScheme("5 ^ 1"), cv.ShouldEqual, "bitXor(5, 1)") // == 4
				cv.So(toScheme("4 | 1"), cv.ShouldEqual, "bitOr(4, 1)")  // == 5
				cv.So(toScheme("5 & 1"), cv.ShouldEqual, "bitAnd(5, 1)") // == 1

				cv.So(toScheme("true && false"), cv.ShouldEqual, "TRUE && FALSE")
				cv.So(toScheme("true || false"), cv.ShouldEqual, "TRUE || FALSE")

				cv.So(toScheme("5 &^ 1"), cv.ShouldEqual, "bitAnd(5, bitFlip(1, bitwidth=32))") // == 4
				cv.So(toScheme("5 != 1"), cv.ShouldEqual, "5 != 1")

			})
		})

		cv.Convey("When we use unary-operations", func() {
			cv.Convey("then we should get the prefix notation.", func() {
				cv.So(toScheme("!false"), cv.ShouldEqual, "!FALSE")
				cv.So(toScheme("!true"), cv.ShouldEqual, "!TRUE")
				cv.So(toScheme("b := -a"), cv.ShouldEqual, "b = -a;")

				// ~5 isn't a legal golang expression, but ^5 means bitwise compliment:
				cv.So(toScheme("^5"), cv.ShouldEqual, "bitFlip(5, bitwidth=32)") // 4611686018427387898
				// -6 in goland, signed.
				// 18446744073709551610 in golang; this is the unsigned, full 64-bits minus 5 version
				// i.e. 2^64 == 18446744073709551616
				// but note that (bitwise-not 5) in scheme is:
				// 4611686018427387898 in scheme, 2 bits (4x) less, indicating scheme is using 62-bit fixnums.
			})
		})

		cv.Convey("When we use floating-point division, that is: a / b, for floating-point a or b", func() {
			cv.Convey("then we should get the infix notation (/ a b).", func() {
				cv.So(toScheme("5.0 / 2.0"), cv.ShouldEqual, "5.0 / 2.0") // floating-point
				cv.So(toScheme("a / b"), cv.ShouldEqual, "a / b")         // floating-point
			})
		})

		cv.Convey("When we call fmt.Printf to display an object", func() {
			cv.Convey("it gets turned into a call to (printf)", func() {
				cv.So(toScheme(`fmt.Printf("hello")`), cv.ShouldEqual, `cat("hello")`)
			})
			cv.Convey("instances of %%v and %%#v get turned into ~A within the formatting string", func() {
				cv.So(toScheme(`fmt.Printf("%v %#v\n",a,b)`), cv.ShouldEqual,
					`cat(paste(sep="", a, " ", b, "\n"))`)
			})
		})

		cv.Convey("When we define an increment function; func incr(x int) int { return x + 1 }", func() {
			cv.Convey("then we should get in R: incr = function(x) { return(x + 1) }", func() {
				cv.So(toScheme("func incr(x int) int { return x + 1 }"), cv.ShouldEqual,
					"incr = function(x) { return(x + 1) }")
			})
		})

		cv.Convey("When we define a simple add function; func add(x, y int) int { return x + 1 }", func() {
			cv.Convey("then we should get in R: add = function(x, y) { return(x + y) }", func() {
				cv.So(toScheme("func add(x, y int) int { return x + y }"), cv.ShouldEqual,
					"add = function(x, y) { return(x + y) }")
				cv.So(toScheme("func add(x, y, z int) int { return x + y + z}"), cv.ShouldEqual,
					"add = function(x, y, z) { return(x + y + z) }")
			})
		})

	})
}
