An intepreter for a dynamic language inspired by Lua and Python, see ``syntax_examples.sph``.

I probably should come up with a different name, one that isn't also used by a widely-known documentation tool...

# Sphinx

Welcome to the Sphinx programming language! Sphinx is (or will be) a dynamically typed programming language that is inspired by Lua and Python, and implemented entirely in Rust!

# Goals

My goal is to have a lightweight, expressive language. At the same time, I also want the language runtime to be decently fast, and the design of the language tries to balance these goals.

# Safe Rust FFI

Because Sphinx is (mostly) implemented in Safe Rust, it should be possible to provide a completely safe FFI with Rust code. This would allow a host Rust application to gain the capabilities of an embedded dynamic scripting language.

As a long term goal I would like to also leverage the rlua bindings to provide a Lua FFI in Sphinx, as well.

# Future Type Inference and Static Type Checking

While it is a bit long term, I would like to eventually have a (dynamic) structurally typed programming language that also includes static type checking using type annotations and inference (in the same vein as PyType). However, for the time being the main focus is on just getting the language up and running. 

The plan is first - an interpreter, then compilation to bytecode and a VM - then static analysis during the bytecode compilation step.

# Syntax Highlighting Support

At the present moment, nearly complete syntax highlighting is available for users of Sublime Text - just copy `sphinx.sublime-syntax` into your user packages directory. If you use a different text editor and want syntax highlighting for Sphinx, feel free to drop a request on GitHub. Getting the language working is my first priority, but I don't mind taking a look at it.

# Examples (not fully implemented yet)
```
#{ 
    Block Comment  
    #{ Nested! }#
}#

echo "Hello, world!"  # print() function will be available later once there is a builtin library

# Semicolons are optional. The syntax has been designed so that the end of a statement can always be inferred.
"One"; "Two"

# Mutable and immutable variables
let immutable = "can't change me"
var mutable = 0
echo mutable += 1  # almost all constructs are expressions

var annotated: Float = 3.14159  # not implemented yet, but someday...

# Tuples
"abc", 123

# Tuple assignment
var first, second = "abc", "def"
second, first = first, second
assert "def", "abc" == first, second

# Objects
{ value = 0xA }  # anonymous object

Person { value = 0xC }  # classy object


# Functions, including default and variadic arguments

# note: default arguments are re-evaluated with each invocation (unlike another scripting language that shall not be named)
fun echo_default(thing = "default")
  echo thing
end

fun variadic_fun(normal_arg, variadic...)
  echo normal_arg  
  for variadic_arg in variadic do
    echo variadic_arg
  end
end

variadic_fun("red", "blue", "green")  # prints "red" then "blue" then "green"

# Note: named arguments are not supported (it helps make function invocation fast)
# Instead, pass an anonymous object.
configure_something({ option1: true, option2: false })

# I plan to add support for named arguments, but all functions will either be
# fully positional-only or named-only.


# Argument unpacking and wrapping decorators
fun trace(wrapped)
  return fun(args...)
    echo "trace"
    wrapped(args...)
  end
end

# "@decorator let/var name = value" is syntactic sugar for "let/var name = decorator(value)"
# And "fun name() ... end" is syntatic sugar for "let name = fun() ... end"
# Put that together and we can do:
@trace
fun increment(x)
  return x + 1
end

# Classes, Metatables
# WIP/TBD

```