# Scala with cats

Learning [cats](https://github.com/typelevel/cats) along with [Scala with Cats](https://underscore.io/books/scala-with-cats/)

## Chapter1

### [Type Class](./cats/src/main/scala/com/github/saint1991/cats/chapter1)

Type class is a design pattern that encapsulates the implementation to process a type T value.
It roughly consists of three components: Type class, Type class instances and interface method.
Interface method takes both type T value and an instance of the type class as its arguments and decouples
concrete implementation to process type T value as a type class instance.
So users can use the method to any type T by supplying type class instances to its implicit scope.

#### implicit resolution of invariance, variance and contravariance

| Type Class Variance           | Invariant | Covariant | Contravariant |
|:-----------------------------:|:---------:|:---------:|:-------------:|
| Supertype instance used?      | No        | No        | Yes           |
| More specific type preferred? | No        | Yes       | No            |
 
 
## Chapter2
 
### [Monoid](./cats/src/main/scala/com/github/saint1991/cats/chapter2/Monoid.scala)
 
Monoid is a type class that has following properties.
 
1. (A, A) => A
2. associative
3. has identity element

`1.` means that Monoid is closed, that means it takes two arguments of the same type (A in above) 
and produces a result that is also the same type of arguments.
 
 
### [Semigroup](./cats/src/main/scala/com/github/saint1991/cats/chapter2/Semigroup.scala)
 
Semigroup is a type class that has following properties
  
1. (A, A) => A
2. associative
  
Comparing with Monoid, Semigroup does not have any identity element.
Conversely, Monoid is also Semigroup.

For instances, `NonEmptyList` and `PositiveNumber` does not have identity element so that it is `Semigroup` but not `Monoid` 
These properties are useful when combining values produced in parallel execution because their completion order is no matter.

## Chapter3

### [Functor](./cats/src/main/scala/com/github/saint1991/cats/chapter3/Functor.scala)
A Functor is a type class taking type constructor as its type parameter that 
transform an element of type A into type B within the context.
Functor `F[A]` has the following property.

1. `(A => B) => F[B]`

Thus, functor chains sequential operations within the closed context.

#### Type Constructor
A type constructor is a constructor that produce regular type by being filled a type placeholder.
`List` is a type constructor that can produce regular type `List[Int]` by filling a placeholder with `Int`.

### [Contravariant Functor](./cats/src/main/scala/com/github/saint1991/cats/chapter3/ContravariantFunctor.scala)
Contravariant Functor has the contramap that prepends an operation.

1. `(B => A) => F[B]`

When given the map function B => A, it can be prepended all methods in `F[B]` thus `F[B]` can be instantiated.
any methods m of `F[B]` can be implemented using map function f: B => A as `f andThen m`. 

B => A means B is subtype of A.
It produces `F[B]` from `F[A]` i.e. `F[A] => F[B]`, `F[B]` is regarded as subtype of `F[A]`.
Therefore it fills covariant's relationship.


### [Invariant Functor](./cats/src/main/scala/com/github/saint1991/cats/chapter3/InvariantFunctor.scala)
Invariant Functor has the imap that transform A in bidirection.

1. `(A <=> B) => F[B]`


## Chapter4

### [Monad](./cats/src/main/scala/com/github/saint1991/cats/chapter4/Monad.scala)
A Monad is a type class taking type constructor as its type parameter. 
Similar to Functor, it sequences computing but it can also begin new computing sequence on the middle of another sequence.
Monad has following properties:

1. pure:    `A => F[A]`
2. flatMap: `()F[A], A => F[B]) => F[B]`

#### Built-in monads in cats

- MonadError
    - handle errors in monadic way
- Eval
    - eager/lazy evaluation and memorization
- Writer
    - separate I/O and computation
- Reader
    - computation with a placeholder to inject dependencies
- State
    - a function that transforms an input state to an output state and then computes a result.


|          |  cats  |     Properties      |
|:--------:|:------:|:-------------------:|
| val      | Now    | eager,memorized     |
| def      | Always | lazy, not memorized |
| lazy val | Later  | lazy, memorized     |
