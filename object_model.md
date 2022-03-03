# ReLox Object Model Implementation Notes



## Python Style Design?

Each object may refer to another object as it's "type"

Attribute lookups that fail on an object defer to the object's "type" object.

Special behaviours are handled by specially named callables on objects. 
Whether the special callable ends up being on an instance itself or on the 
instance's "type" object doesn't matter.



## Lua Style Design?

#### Metatables

Every object has a metatable. Multiple objects may refer to the same metatable.

The set of allowed keys and the permitted value for each keys is fixed 
and defined by the language specification. Most keys are optional.

Each key has a specified meaning, and its value is used to customize
the behaviour of the associated object(s).

Metatable keys are referred to either as metafields or metamethods, 
depending on whether or not the associated value is a function.



## Special/MetaMethods

### General

- `__getindex` attributes that are not found on an object are looked up on the object referenced
- `__setindex` assignment
- `__delindex` 

- `__bool` truth value of an object
- `__tostring`

### Context Managers

- `__exit`

### Descriptors

- `__get`
- `__set`
- `__del`


#### Operator Overloading

Many metamethods effectuate operator overloading.

*Unary metamethods*:

- `__pos`
- `__neg`
- `__inv`

*Arithmetic metamethods*:

- `__add`
- `__sub`
- `__mul`
- `__div`
- `__mod`

*Bitwise and Shift metamethods*:

- `__and`
- `__xor`
- `__or`
- `__lshift`
- `__rshift`

*Comparison metamethods*

- `__lt`
- `__le`
- `__eq`


