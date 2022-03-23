An intepreter for a dynamic language inspired by Lua and Python, see ``syntax_examples.sph``.

# Sphinx

Welcome to the Sphinx programming language! Sphinx is (or will be) a dynamically typed programming language that is inspired by Lua and Python, and implemented entirely in Rust!

The goal is to eventually have a (dynamic) structurally typed programming language that also includes static type checking using type annotations and inference (in the same vein as PyType).
However, for the time being the main focus is on just getting the language up and running. 

The plan is first - an interpreter, then compilation to bytecode and a VM - then static analysis during the bytecode compilation step.

# Safe Rust FFI

Because Sphinx is (mostly) implemented in Safe Rust, it should be possible to provide a completely safe FFI with Rust code. This would allow a host Rust application to gain the capabilities of an embedded dynamic scripting language.

As a long term goal I would like to also leverage the rlua bindings to provide a Lua FFI in Sphinx, as well.

# Goals

Sphinx is not a Lua or Python clone, though it's syntax draws inspiration from both of these languages (more so from Lua than from Python, especially the excellent paper on the implementation of Lua 5.0). My goal is to have a lightweight, expressive language. At the same time, I also want the language runtime to be fast, and the design of the language has centered around balancing these goals.

For example, while module-level variables do end up in a HashMap, all local variables are stored in a stack and referred to by index - which is all sorted out at (bytecode) compile time. Similarly, function calls are fairly low overhead.

# Syntax Highlighting Support

At the present moment, nearly complete syntax highlighting is available for users of Sublime Text - just copy `sphinx.sublime-syntax` into your user packages directory. If you use a different text editor and want syntax highlighting for Sphinx, feel free to drop a request on GitHub. Getting the language working is my first priority, but I don't mind taking a look at it.

# Acknowledgement

I want to acknowledge Robert Nystrom for his wonderful book on [Crafting Interpreters](https://craftinginterpreters.com), which was an inspiration for starting this project and provided some direction while getting started. That said, I do also want it to be clear that Sphinx is not related to the language developed in that book, and been designed and developed independently from the ground up.
