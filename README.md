An intepreter for a dynamic language inspired by Lua and Python, see ``syntax_examples.sph``.

Mainly a learning project, this is heavily WIP.

# Sphinx

Welcome to the Sphinx programming language! Sphinx is (or will be) a dynamically typed programming language that is heavily inspired by Lua and Python, and implemented entirely in Rust!

The goal is to eventually have a (dynamic) structurally typed programming language that also includes static type checking using type annotations and inference (in the same vein as PyType).
However, for the time being the main focus is on just getting the language up and running. 

The plan is first - an interpreter, then compilation to bytecode and a VM - then static analysis during the bytecode compilation step.

# Safe Rust FFI

Because Sphinx is implemented entirely in Safe Rust, it should be possible to provide a completely safe FFI with Rust code, adding the capabilities of a dynamic scripting language to a host Rust application.

As a long term goal I would like to also leverage the rlua bindings to provide a Lua FFI in Sphinx, as well.
