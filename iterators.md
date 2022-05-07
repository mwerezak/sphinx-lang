# Iterators in Sphinx

Iterators are a mechanism used for producing sequences of values. 
They are used to support `for` loops and `...` argument unpacking syntax.

## Iterators and Iterables

An *iterator* in Sphinx is anything that supports the `__next` metamethod.

An *iterable* is one of the following:
- A callable that when invoked with no arguments returns an iterator.
- An object that has the `__iter` metamethod which returns am iterator.
- A plain iterator is also an iterable.

`__next` takes no arguments and returns a tuple `(next, ...)` or `nil`.



`invariant` is a value that is passed to each invocation of `iterfunc`

`i0` is a value that is passed to `iterfunc` as the initial value of `i`

## More Details


## Usage

Iterators can be used in a `for` loop like so:
```
for item in create_iter() do
  print(item)
end
```

