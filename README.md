An intepreter for a dynamic language inspired by Lua and Python, see ``syntax_examples.sph``.

Please note that the name is provisional and will change. (Yes, I know about the reST documentation tool by the same name, having used it myself. I just liked the name).

# Sphinx

Welcome to the Sphinx programming language! Sphinx is (or will be) a dynamically typed programming language that is inspired by Lua and Python, and implemented entirely in Rust!

The goal is to eventually have a (dynamic) structurally typed programming language that also includes static type checking using type annotations and inference (in the same vein as PyType).
However, for the time being the main focus is on just getting the language up and running. 

The plan is first - an interpreter, then compilation to bytecode and a VM - then static analysis during the bytecode compilation step.

# Safe Rust FFI

Because Sphinx is (mostly) implemented in Safe Rust, it should be possible to provide a completely safe FFI with Rust code. This would allow a host Rust application to gain the capabilities of an embedded dynamic scripting language.

As a long term goal I would like to also leverage the rlua bindings to provide a Lua FFI in Sphinx, as well.

# Goals

Sphinx is not a Lua or Python clone, though it's syntax draws inspiration from both of these languages (more so from Lua than from Python). 
One difference from both of these languages is that Sphinx is highly expression-oriented. With the exception of loops, most language constructs are expressions.

My goal is to have a lightweight, expressive language. At the same time, I also want the language runtime to be decently fast, and the design of the language has involved balancing these goals.

# Syntax Highlighting Support

At the present moment, nearly complete syntax highlighting is available for users of Sublime Text - just copy `sphinx.sublime-syntax` into your user packages directory. If you use a different text editor and want syntax highlighting for Sphinx, feel free to drop a request on GitHub. Getting the language working is my first priority, but I don't mind taking a look at it.
