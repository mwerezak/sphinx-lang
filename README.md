# Sphinx

*The lastest version (0.5) requires nightly Rust. This is because pointer metadata is being used to GC unsized types, whereas before a double-indirection was required.*

Sphinx is a dynamically typed programming language that is inspired by Lua and Python, and implemented entirely in Rust!

Sphinx is not complete! There is still a lot to do before it is a functional language. Some things on my radar:

 - import system
 - a basic standard library
 - work out the details of the class/object system

# Goals

My goal is to have a lightweight, expressive language. At the same time, I also want the language runtime to be decently fast, and I aim to balance these two goals.

# Some Things That I Like About the Implementation

I have an internal type system built around a `MetaObject` trait which makes it easy to specify the behaviours that are supported by each of the core primitive types in the language. Thanks to Rust's enum types (and some macros) this is implemented without any vtables, using static dispatch based on the enum discriminant.

Different representations of string data. Short strings are "inlined" on the stack when possible (inspired by flexstr). All strings that are used as identifiers are interned. Only strings >40 bytes long are allocated using the GC. The bytecode compiler converts local variable names into indexes into the value stack, so strings are not used at all when referencing local variables.

The Sphinx language uses a simple Mark-Trace GC inspired by `rust-gc`. The *runtime* itself does not use any GC. Since we are only GC'ing script data, a lot of the challenges that would be involved in writing a GC for general Rust code are avoided (that for example, `rust-gc` has to deal with).

The GC also supports allocating dynamically sized types without double-indirection (i.e. the `Gc<T>` smart pointer used for GCed data points directly to the DST, not a Box). It also uses *thin pointers* to refer to the dynamically sized allocation, which keeps the size of each `Gc<T>` down to a single `usize`. The GC also supports weak references!

In the future I would like to support incremental GC and *maybe* generational GC as well, but the current implementation is simple and works pretty well.

# Safe Rust FFI

Because Sphinx is (mostly) implemented in Safe Rust, it should be possible to provide a completely safe FFI with Rust code. This would allow a host Rust application to gain the capabilities of an embedded dynamic scripting language.

As a long term goal I would like to also leverage the rlua bindings to provide a Lua FFI in Sphinx, as well.

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
echo mutable += 1  # almost all constructs in Sphinx are expressions

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

# Note: named arguments are not supported. It may be added in the future.
# You can pass an anonymous object instead.
configure_something({ option1: true, option2: false })


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
