The Deturgenchry Programming Language
=====================================

_Deturgenchry_ is a simple object-oriented language with several
distinguishing features.  It is a work in progress, so exactly what those
features are is still being hashed out, but they're something like:

* Single-assignment: all objects are immutable.
* The implicit `self` parameter is a continuation (or similar) representing
  the currently executing method; `self.object` is the current object.
* There is also an `other` parameter which represents the method that
  called the current method.  There is no explicit `return`; instead,
  `other` is re-activated.

For more detailed information on the language (so far), and a set of
Falderal tests, see `doc/Deturgenchry.markdown`.

The reference implementation of Deturgenchry is written in Haskell,
and requires the `Parsec` parser combinator library.

The Deturgenchry project is covered under a BSD-style license; see the
file `LICENSE` for more information.
