// this would be a comment, I assume.
/* this too */
/* no /* or // comment nesting apparently */
print("hello, v8") // semicolons optional I guess
print("goodbye")

var qux = "hello"
function foo(bar) {
	print(qux + " " + bar)
}

foo("again")

qux = "goodbye"

foo("again")

var qux = "is this the same qux?"

foo("yes")

function bar() {
	print("does this even need to be in scope right now? " + quux)
}

var quux = "I guess not.  That's creepy."

bar()

function hmm(x) {
	var it = x
}

function ouch(pain) {
	it = pain
}

var it = "ok"
hmm("???")
print(it)

ouch("not ok")
print(it)