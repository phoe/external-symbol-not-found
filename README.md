# EXTERNAL-SYMBOL-NOT-FOUND

This is a minor portability library that allows the user to check if a condition
has been signaled due to trying to read a non-existing or non-external symbol of
a package when the `foo:bar` notation was used.

These two test cases illustrate the condition that would be signaled:

```common-lisp
(make-package 'foo :use nil)
(read-from-string "foo:bar")
```

```common-lisp
(make-package 'foo :use nil)
(intern "BAR" 'foo)
(read-from-string "foo:bar")
```

Note that `EXTERNAL-SYMBOL-NOT-FOUND` is a Common Lisp type but it is *not* a
Common Lisp condition type.

You cannot create new instances of this condition, but you can
refer to already existing instances created by your Lisp implementation using
this type; most importantly, this type is valid for usage in `HANDLER-CASE`,
`HANDLER-BIND` and other similar macros where it is required to specify a type
for later type matching.

## Supported implementations

Implemented for and tested on:
  * SBCL 1.4.6
  * CCL 1.11.5
  * ABCL 1.5.0
  * ECL 16.1.2

On all other implementations, the system will load, but using any of this
project's functionality will signal an error.

Adding support for other implementations is easy. If you do not want to do it
yourself, please make an issue on this project along with the output of the
following Lisp form:

```common-lisp
(handler-case (progn
                (unless (find-package 'temp)
                  (make-package 'temp :use nil))
                (read-from-string "temp:symbol"))
  (error (e)
    (fresh-line)
    (format t "~A~%" e)
    (describe e)
    (describe (class-of e))))
```

## Exports

  * Type `EXTERNAL-SYMBOL-NOT-FOUND` - denotes conditions signaled when the Lisp
  reader encountered a non-existing external symbol of a package.
  * Predicate `EXTERNAL-SYMBOL-NOT-FOUND-P` - returns true iff the condition
  is of type `EXTERNAL-SYMBOL-NOT-FOUND`.
  * Function `EXTERNAL-SYMBOL-NOT-FOUND-SYMBOL-NAME` - retrieves the symbol name
  from the condition instance.
  * Function `EXTERNAL-SYMBOL-NOT-FOUND-PACKAGE` - retrieves the package object
  from the condition instance.

## License

Unlicense. Do whatever you want with it, the code is here to be useful.
