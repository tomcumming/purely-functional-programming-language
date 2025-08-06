This project will be a prototype purely functional programming language that
looks and feels a lot like [Haskell](https://www.haskell.org/) but will be
strict and linear by default.

### Some very rough ideas:

#### Linear

As the language is linear, all named values must be used exactly once. If a
name is used less than once, the compiler will insert a drop for you:
  - `\x -> ()` ==> `\x -> let () = drop x in ()`

If a name is used more than once the compiler will copy it for you:
  - `\x -> x x` ==> `\x -> let (x, x') = copy x in x x'`

```
class Drop a where
  drop : a -> ()

class Copy a where
  copy : a -> (a, a)
```

#### Functions

The type of functions looks like `a % b -> c` where:
  - `a` is the type of the closure
  - `b` is the input type
  - `c` is the return type

Closure types being exposed means closures can live on the stack. Closures can
be copied and dropped if the types they close over implement the `Copy` and
`Drop` classes.

Because I want currying to work (and look) close to Haskell, I will have to
come up with some notation so that curried functions are quick to read:
  - Term: `\(w : a) -> \(x : b) -> \(y : c) -> (f a b c : d)`
  - Raw Type: `() % a -> (a % b -> ((a, b) % c -> d))`
  - Notation: `a --> b --> c --> d`

The closure being part of the type means we can't unify functions with
different closure types, this might be very annoying but I can provide some
ways to help:
  - Boxing of functions (moving the closed over part to the heap)
  - Helpers to construct closure with specific type

```
Ur : Type
Linear : Type

instance Drop Ur; instance Copy Ur;

Ur.box : (Copy a, Drop a) => (a % b -> c) --> (Ur % b -> c)
Linear.box : (a % b -> c) --> (Linear % b -> c)

closeOver : a --> (() % (a, b) -> c) --> (a % b -> c)
```

#### Polymorphism

Unlike Haskell, all general purpose type variables must be at the root of the
type, AKA we allow for type schemes but not unrestricted higher-rank
polymorphism.

The benefits of this are always knowing the size of values, and having to do
very little boxing. Type-class dictionaries will be completely static and
in-lined.

I hope to mitigate some of the drawbacks by allowing some higher rank
polymorphism but restricted to kinds that are not 'value's, only kinds that
specify regions and other things which can be completely eliminated before code
generation. 

So we might have some constructions that look like `runST`:

```
withRef : a --> (forall r. Ref r a --> b) --> (a, b)
```

(Note that `Ur.box` above would potentially allow the `Ref` to be used outside
of the region, so we probably need operations on `Ref`s to be effectful or some
other design)

#### Effects and IO

The IO monad as implemented in Haskell is probably not a good idea when the
language has the current restrictions. This is because IO is essentially a
function `RealWorld -> (RealWord, a)`, and this would mean we need to include
the closure required to evaluate this function.

In linear languages people tend to thread through the world explicitly:

```
sayHello = \w ->
  let (w, name) = readLine "What is your name?" w
  in printLine ("Hello " <> name) w
```

This will be very annoying, but hopefully we can perform something similar to
the 'Monadic bang' transform:

```
sayHello = thread \() ->
  let name = ! readLine "What is your name?"
  in ! printLine ("Hello " <> name)
```

This could also allow for a fairly pleasant effect system for all effects that
can be implemented via threading a value, such as:
  - IO
  - Reader
  - Writer
  - State

We can then combine them and allow for something like `mtl` without much
trouble.
