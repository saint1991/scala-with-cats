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
 
### [Monoid](./cats/src/main/scala/com/github/saint1991/cats/chapter2)
 
Monoid is a type class that has following properties.
 
1. (A, A) => A
2. associative
3. has identity element

`1.` means that Monoid is closed, that means it takes two arguments of the same type (A in above) 
and produces a result that is also the same type of arguments.
 
 
### [Semigroup](./cats/src/main/scala/com/github/saint1991/cats/chapter2)
 
Semigroup is a type class that has following properties
  
1. (A, A) => A
2. associative
  
Comparing with Monoid, Semigroup does not have any identity element.
Conversely, Monoid is also Semigroup.

For instances, `NonEmptyList` and `PositiveNumber` does not have identity element so that it is `Semigroup` but not `Monoid` 
These properties are useful when combining values produced in parallel execution because their completion order is no matter.
