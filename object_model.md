# Sphinx Object Model Implementation Notes




## Proposal

Every object has a metatable. Multiple objects may refer to the same metatable.

Special behaviours are handled by specially named callables in the object's metatable. 
The metatable is just an object, and typically shared between all instances of a class.
It is up to the class object to populate an instance's metatable when initializing a new instance.

(A class is just an object that can be used to create "instances" - other objects. The only thing
tying an object to it's class is the default implementation of the `__getindex` metamethod in that
object's metatable.)

Using a metatable object in this way avoids the need for metaclasses a la Python.
It also allows objects to act like dictionaries by default, since any special methods used for indexing
wont count towards len() (since they are on the metatable, not the object).

By default, methods in the class that match special metamethod names (e.g. `__getindex`) will
be populated into a metatable that is cached somewhere in the class object and given to new instances. 
Otherwise, such specially named methods have no special signficance above any other method on an 
object being used as a class.

Attempting to insert a key into a metatable that is not one of the keys defined by the language
itself should result in an error.

### Notable metamethods:

`__new` - objects that have this metamethod are class objects and can be used to create new instances
using object initializer syntax (`Class { ... }`). By default it just sets the new instance's metatable
to the object produced by the `__meta` metamethod.

`__meta` - an object (or a callable that creates an object) that will be set as the metatable for
any new instances created using `__new`. If not present then the default object metatable will be used.

`__getattr` - a callable that is invoked when `.` access fails on an object. 
By default it looks for the item on the class obj before looking at parent classes
following a Python-like mro. If not present then `__getindex` will be called on the object instead.

`__getindex` - a callable that is invoked when indexing fails on an object.
Unlike `__getattr`, the default object metatable does not contain an entry for `__getindex`.
This means that by default indexing is local to an instance, it does not attempt to look up keys
in the class object unless overridden.

`__class` - used by the default implementation of `__getattr` obtain the class object.

`__mro` - used by the default implementation of `__getattr` to iteratively search parent classes. 
By default should use C3 Linearization.

### Class definition expressions

Class definition expressions are just syntactic sugar, they are equivalent to:

1. creating a new default object (which you can do just using `{ ... }` without specifying a class), and then...
2. setting up that new object's metatable with the appropriate metamethods to act as an instance factory, then...
3. inserting methods and fields into the new class object that were contained in the class definition expression,
and then finally...
4. (to support the descriptor protocol) searching for any values that were inserted into the new class object,
and seeing if any of them are objects that have `__set` in their metatable. If any do, their `__set` metamethod
is invoked to give the descriptor the opportunity to initialize using the newly created class object.



## Special/Meta-methods

### General

- `__getattr`
- `__setattr`
- `__delattr`

- `__getindex` 
- `__setindex` 
- `__delindex` 

Note: while there are separate metamethods for attr and index access, 
by default all objects are dictionaries like in Lua and support both attr access and indexing.

- `__bool` truth value of an object
- `__tostring`

- `__new`
- `__meta`
- `__class`
- `__mro`

### Context Managers

- `__exit`

### Descriptors

- `__get`
- `__set`
- `__del`

Note: when class is created using a class-def expression, any class members will have their `__set` metamethod invoked


### Operator Overloading

Many metamethods effectuate operator overloading.

*Unary*:

- `__pos`
- `__neg`
- `__inv`

*Arithmetic*:

- `__add`, `__radd`
- `__sub`, `__rsub`
- `__mul`, `__rmul`
- `__div`, `__rdiv`
- `__mod`, `__rmod`

*Bitwise and Shift*:

- `__and`, `__rand`
- `__xor`, `__rxor`
- `__or`, `__ror`
- `__shl`, `__rshl`
- `__shr`, `__rshr`

*Comparison*

- `__lt`
- `__le`
- `__eq`





## Rejected Ideas


### Candidate 1: Python Style Design?

Each object may refer to another object as it's "type"

Attribute lookups that fail on an object defer to the object's "type" object.

Special behaviours are handled by specially named callables on objects. 
Whether the special callable ends up being on an instance itself or on the 
instance's "type" object doesn't matter.

To customize behaviour on a class object itself you need to use a metaclass.


### Candidate 2: Lua Style Design?

Every object has a metatable. Multiple objects may refer to the same metatable.

The set of allowed keys and the permitted value for each keys is fixed 
and defined by the language specification. Most keys are optional.

Each key has a specified meaning, and its value is used to customize
the behaviour of the associated object(s).

Metatable keys are referred to either as metafields or metamethods, 
depending on whether or not the associated value is a function.