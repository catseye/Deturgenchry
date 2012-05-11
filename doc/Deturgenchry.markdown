The Deturgenchry Programming Language
=====================================

WORK IN PROGRESS  
Version 0.x, sometime in the twenty-tens  
Chris Pressey, Cat's Eye Technologies

Introduction
------------

_Deturgenchry_ is a simple object-oriented language with several
distinguishing features.

First, Deturgenchry is a _single-assignment_ language.  Neither local (to a
method) bindings, nor the members of an object, are mutable: they may not
be changed.  Another local binding with a new name must be used, or another
instance of the object (with a new value substituted for the given member)
must be created.

Second, the implicit parameter `self` passed to a method does not refer
directly to the object instance on which the method was invoked; it refers
to the *method* instance, currently executing.  To get to the object
instance you have to say `self.object`.  And, since this is the currently
executing method instance, this is maybe where the local variables live:
`self.x`, `self.y`, etc.

Third, unlike most OO languages where only a single implicit `self`
parameter is passed, in Deturgenchry *two* parameters are passed implicitly:
the `self` and the `other`.  This dark symmetry is in honour of modern
psychoanalytic mumbo-jumbo or whatever.  The `other` refers to the method
instance that called the currently executing method.

Fourth, there is no `return` statement.  Instead, the `other` is
reactivated.

This could possibly lead to a restriction: each method may contain only
*one* call site for any given method.  That is, no method may contain more
than one call to any given method inside its definition.  This allows the
correct "return zone" to be known when re-activating the `other`: it is just
after the (unique, we now know) location of the call to the current method.

(Of course, there may be a problem with this if the `other` is stored
somewhere and passed to a method that the `other` did not directly call...)

Example
-------

    class Junk {
      method do_it(n) {
        self' = (self.x = n)
        pass other {self', self.y}
      }
    }

"Discussion"
------------

Recall that one way to make a recursive anonymous function /x/ is to pass
/x/ as the first argument of the function.  The function then invokes this
parameter to recurse.

This opens up a few possibilities.  This first argument can be made implicit
and can be called `self`.  Further, self need not always be self exactly
(like how `self` can be a subclass in OO code.)  Further still, `self` could
be a continuation which is (semi-)implicitly continued.

Grammar
-------

    Program      ::= {ClassDefn}.
    ClassDefn    ::= "class" name "{" {MethodDefn} "}".
    MethodDefn   ::= "method" name "(" [name {"," name}] ")" Statement.
    Statement    ::= Block | Conditional | Transfer | Assignment.
    Block        ::= "{" {Statement} "}".
    Conditional  ::= "if" Expr Statement "else" Statement.
    Transfer     ::= "pass" Expr Expr.
    Assignment   ::= name<new,local> "=" Expr.
    Expr         ::= RefExpr
                   | "new" name<class>
                   | IntegerLiteral.
    RefExpr      ::= name<local> {"." name} [SetExpr | CallExpr].
    SetExpr      ::= "[" name "=" Expr {"," name "=" Expr} "]".
    CallExpr     ::= "(" Expr {"," Expr} ")"].

Examples
--------

This is a bit of an experiment in test-driven language design.  I have
a rough idea for how the language should work, so I'm going to
write a bunch of programs in the language, as tests for an
interpreter which doesn't exist yet.  Then I'm going to write
the interpreter to make the tests pass.

    -> Functionality "Interpret Deturgenchry Program" is implemented by
    -> shell command "./deturgenchry %(test-file)"

    -> Tests for functionality "Interpret Deturgenchry Program"

Tiara Evaluation
----------------

A program consists of zero or more class definitions.  When a program
is run, a class named `Main` is sought, an instance of it is created,
and the nullary method `main` on it is invoked.

    | class Main {
    |   method main() {
    |   }
    | }
    = Null

    | class Arbitrage {
    |   method main() {
    |   }
    | }
    ? No Main class with main() method found

    | class Main {
    |   method harangue() {
    |   }
    | }
    ? No Main class with main() method found

    | class Main {
    |   method main(n) {
    |   }
    | }
    ? Too few parameters passed to method

The main method may return control to the operating system (or whatever
started running the program) by passing a value to `other`.

    | class Main {
    |   method main() {
    |     pass other 5
    |   }
    | }
    = IntVal 5

Local variables may be assigned values.

    | class Main {
    |   method main() {
    |     k = 5
    |   }
    | }
    = Null

Local variables may be referenced in expressions.

    | class Main {
    |   method main() {
    |     k = 5
    |     pass other k
    |   }
    | }
    = IntVal 5

Code in a method may instantiate objects of any class.

    | class Junk {}
    | class Main {
    |   method main() { pass other new Junk }
    | }
    = ObjVal "Junk" []

Classes don't have to have been defined yet to be referenced.

    | class Main {
    |   method main() { pass other new Junk }
    | }
    | class Junk {}
    = ObjVal "Junk" []

Classes don't even have to be defined at all, to be referenced.
They're assumed to be "plain" classes in this case, with no relationship
to any other classes.

    | class Main {
    |   method main() { pass other new Junk }
    | }
    = ObjVal "Junk" []

A class may contain methods.  A method may be invoked on an instance
in the usual fashion.

    | class Junk {
    |   method fire(n) {
    |     pass other n
    |   }
    | }
    | class Main {
    |   method main() {
    |     o = new Junk
    |     k = o.fire(2)
    |     pass other k
    |   }
    | }
    = IntVal 2

The number of actual parameters passed to a method must match the number
of formal parameters the method declares.

    | class Main {
    |   method fire(a,b,c) { pass other b }
    |   method main() {
    |     o = new Main
    |     k = o.fire(4,5,6)
    |     pass other k
    |   }
    | }
    = IntVal 5

    | class Main {
    |   method fire(a,b,c) { pass other b }
    |   method main() {
    |     o = new Main
    |     k = o.fire(4,5)
    |     pass other k
    |   }
    | }
    ? Too few parameters passed to method

    | class Main {
    |   method fire(a,b,c) { pass other b }
    |   method main() {
    |     o = new Main
    |     k = o.fire(4,5,6,7)
    |     pass other k
    |   }
    | }
    ? Too many parameters passed to method

A method has access to the currently executing method: `self`.
This value is actually a continuation.

    | class Junk {
    |   method fire() {
    |     pass other self
    |   }
    | }
    | class Main {
    |   method main() {
    |     o = new Junk
    |     pass other o.fire()
    |   }
    | }
    = ContVal (ObjVal "Junk" []) "fire" []

Methods provide access to the object they're attached to.

    | class Junk {
    |   method fire() {
    |     pass other self.object
    |   }
    | }
    | class Main {
    |   method main() {
    |     o = new Junk
    |     pass other o.fire()
    |   }
    | }
    = ObjVal "Junk" []

Deturgenchry is single-assignment.  It is not possible to assign to any
parameter to the current method, or any variable that has already
been bound, or to `self` or `other`.

    | class Main {
    |   method main() {
    |     o = new Main
    |     o = new Main
    |     pass other o
    |   }
    | }
    ? Attempted re-assignment of bound name o

    | class Main {
    |   method main() {
    |     self = new Main
    |     pass other self
    |   }
    | }
    ? Attempted re-assignment of bound name self

    | class Main {
    |   method main() {
    |     other = new Main
    |     pass other new Main
    |   }
    | }
    ? Attempted re-assignment of bound name other

The standard if-else construct is a standard enough conditional.  There is no
boolean type; there is only a special Null value, which represents falsehood.
Everything else is truthy.

    | class Bubkis {
    | }
    | class Main {
    |   method main() {
    |     if self {
    |       pass other new Main
    |     } else pass other new Bubkis
    |   }
    | }
    = ObjVal "Main" []

A method which doesn't pass anything back to other implicitly passes Null
back to other.

    | class Bubkis {
    |    method fantastic() {}
    | }
    | class Main {
    |   method main() {
    |     b = new Bubkis
    |     if b.fantastic() {
    |         pass other new Main
    |     } else pass other new Bubkis
    |   }
    | }
    = ObjVal "Bubkis" []

There is a built-in class called StdLib.  Instances of this class expose
methods to do common useful things, like arithmetic.

    | class Main {
    |   method main() {
    |     stdlib = new StdLib
    |     pass other stdlib.gt(4, 5)
    |   }
    | }
    = Null

This is a complicated, contrived, syntactically correct Deturgenchry program.

    | class binkie {
    |   method foo() {
    |       if self {} else {}
    |   }
    |   method bar(whee, y) {
    |       if y
    |         m = whee.x
    |       else {
    |         m = whee[x = y]
    |       }
    |       pass other m
    |   }
    | }
    | class schmoo {
    | }
    | class Main {
    |   method main() {
    |     pass other new Main
    |   }
    | }
    = ObjVal "Main" []
