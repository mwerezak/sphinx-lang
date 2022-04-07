# Iterators in Sphinx

Iterators are a mechanism used for producing sequences of values. 
They are used to support `for` loops and `...` argument unpacking syntax.

An iterator is one of the following:
- A 3-tuple: `(iterfunc, i0, invariant)`
- A callable that when invoked with no arguments creates a 3-tuple like the one above
- An object that has the `__iter` metamethod which returns a 3-tuple like the one above

`iterfunc` must be a callable that accepts two arguments: `(i, invariant)` and returns a tuple `(next_i, ...)` or `nil`.

`invariant` is a value that is passed to each invocation of `iterfunc`

`i0` is a value that is passed to `iterfunc` as the initial value of `i`

Iterators can be used in a `for` loop like so:
```
for item in create_iter() do
  echo item
end
```

