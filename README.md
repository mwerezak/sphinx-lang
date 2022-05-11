# Sphinx

Sphinx is a dynamically typed programming language that is inspired by Lua and Python, and implemented entirely in Rust!

Sphinx is not complete! It is in a pre-alpha state of development and there is still a lot to do before it is a fully functional language. Some things on my radar:

 - import system
 - a basic standard library
 - work out the details of the class/object system

# Goals

My goal is to have a lightweight, expressive language. At the same time, I also want the language runtime to be reasonably fast.

As well, this whole project started as a way to learn Rust, and I have definitely been doing a lot of that. I would also love for this to be an actual, practical, and complete development tool for someone someday, and that's still a fairly long road to walk.

# Ok, how do I run it?

Sphinx makes use of Rust's [pointer metadata API](https://github.com/rust-lang/rust/issues/81513), which has not yet been stabilized. So in order to build it you will need nightly Rust. Probably if you're here you're interested in looking at the internals of a compiler/VM (since the language itself is pretty WIP), so you probably already know how to set that up, but if you don't, you can get it with `rustup`. 

Once built, you can run the REPL with `sphinx` and the disassembler with `sphinx-dasm`. Both executables have `--help` to list the command line options. Also check out the `--debug` option on `sphinx` which allows you to step through each instruction and view the state of the VM. Below is some example code you can run to get started:

If you run the REPL, the `globals()` function will allow you to see what builtins are currently available. There is a `help()` function, though it isn't fully supported yet. Currently it only accepts functions and will print out the function signature.

```
# Makes a counter using closures
fun make_inc()
    var a = 0
    fun inc(s = 1)
        nonlocal a += s
    end
end

# Warning - very slow! This was intended for a stress test.
# You will definitely notice a speedup if compile Sphinx in release mode.
fun fib(n)
    if n < 2 then 
        return n 
    end
    
    fib(n - 2) + fib(n - 1)
end

# Some fun with tuple assignment
# declare "c" and "d" as mutable, the rest as immutable, and capture any excess in a tuple
let a, b, (var c, d), e, rest... = 1, 2, (3, 4), 5, 6, 7
c += (d *= 2)
assert a, b, c, d, e == (1, 2, 11, 8, 5)
assert rest == (6, 7)

```

You can also take a look at some of the test scripts inside the `tests` folder.

Right now `sphinx` will compile the code and execute it from memory. I plan to add support for binary bytecode input/output, but right now even the file format for that is TBD

# Some things that I like about the Implementation

I have an internal type system built around a `MetaObject` trait which makes it easy to specify the behaviours that are supported by each of the core primitive types in the language. Thanks to Rust's enum types (and some macros) this is implemented without any vtables, using runtime dispatch based on the enum discriminant.

Different representations of string data. Short strings are "inlined" on the stack when possible (inspired by flexstr). All strings that are used as identifiers are interned. Only strings >40 bytes long are allocated using the GC. The bytecode compiler converts local variable names into indexes into the value stack, so strings are not used at all when referencing local variables.

The Sphinx language uses a simple Mark-Trace GC inspired by `rust-gc`. The *runtime* itself does not use any GC. Since we are only GC'ing script data, a lot of the challenges that would be involved in writing a GC for general Rust code are avoided (that for example, `rust-gc` has to deal with).

The GC also supports allocating dynamically sized types without double-indirection (i.e. the `Gc<T>` smart pointer used for GCed data points directly to the DST, not a Box). It also uses *thin pointers* to refer to the dynamically sized allocation, which keeps the size of each `Gc<T>` down to a single `usize`. The GC also supports weak references!

In the future I would like to support incremental GC and *maybe* generational GC as well, but the current implementation is simple and works pretty well.

# Is it a compiled language or an interpreted language?

Sphinx is compiled to a bytecode intermediate representation which is run on a virtual machine. In the future I would like to move to a JIT implementation but there are a lot of other things to deal with first.

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

print("Hello, world!")

# Semicolons are optional. The syntax has been designed so that the end of a statement can always be inferred.
"One"; "Two"

# Mutable and immutable variables
let immutable = "can't change me"
var mutable = 0
print(mutable += 1)  # almost all constructs in Sphinx are expressions

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

# note: default arguments are re-evaluated with each invocation (unlike Python)
fun echo_default(thing = "default")
    print(thing)
end

fun variadic_fun(normal_arg, variadic...)
    print(normal_arg  )
    for variadic_arg in variadic do
        print(variadic_arg)
    end
end

variadic_fun("red", "blue", "green")  # prints "red" then "blue" then "green"

# Note: named arguments are not supported. It may be added in the future.
# You can pass an anonymous object instead.
configure_something({ option1 = true, option2 = false })


# Argument unpacking and wrapping decorators
fun trace(wrapped)
    return fun(args...)
        print("trace")
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

