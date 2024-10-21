![GitHub Release](https://img.shields.io/github/v/release/goshacodes/backstub?color=blue])

## Backstub

1. Tired of choosing between mocks and stubs?
2. Tired of argument matchers and thrown exceptions when expectations not met?
3. Or you just don't like 'mocks/stubs' approach entirely?

Then you've come to the right place, meet **backstub** - stubbing library for Scala 3 inspired by [scalamock](https://github.com/paulbutcher/ScalaMock) and compatible with any testing framework out of the box

## Setup

### Basic
Add to your build.sbt

```scala
libraryDependencies += "io.github.goshacodes" %% "backstub" % "<version_from_badge>"
```

Library hugely relies on experimental scala 3 features, so consider also adding

```scala
Test \ scalacOptions += "-experimental"
```

### ZIO

For ZIO integration also add:
```scala
libraryDependencies += "io.github.goshacodes" %% "backstub-zio" % "<version_from_badge>"
```

### Cats Effect

For Cats Effect integration also add:
```scala
libraryDependencies += "io.github.goshacodes" %% "backstub-cats-effect" % "<version_from_badge>"
```

## API

### Basic Stubs

Should be mixed with your test-suite, to provide clean-up API if you create your stubs per suite

```scala 3
package backstub

trait Stubs:
  final given stubs: CreatedStubs = CreatedStubs()

  final def resetStubs(): Unit = stubs.clearAll()
```

Using it as simple as:

```scala 3
import backstub.*

class MySpec extends munit.FunSuite with Stubs:
  override def afterEach(context: AfterEach) =
    resetStubs()
    
```

### ZIO Stubs

Gives you instance of StubEffect, allowing to integrate ZIO

```scala 3
package backstub

import zio.*

trait ZIOStubs extends Stubs:
  // this method is actually in Stubs
  final def resetStubsIO[F[+_, +_]: StubEffect]: F[Nothing, Unit] =
    summon[StubEffect[F]].unit(resetStubs())
    
  given effect.StubEffect[IO] with
    def unit[T](t: => T): UIO[T] = ZIO.succeed(t)
    def flatMap[E, EE >: E, T, T2](fa: IO[E, T])(f: T => IO[EE, T2]): IO[EE, T2] = fa.flatMap(f)


```

### Cats Stubs

Gives you instance of StubEffect.Mono, allowing to integrate Cats Effect

```scala 3
package backstub

import cats.effect.*

trait CatsEffectStubs extends Stubs:
  // this method is actually in Stubs
  final def resetStubsF[F[+_]: StubEffect.Mono]: F[Unit] =
    summon[StubEffect.Mono[F]].unit(resetStubs())

  given StubEffect.Mono[IO] = new StubEffect.Mono[IO]:
    def unit[T](t: => T): IO[T] = IO(t)
    def flatMap[E, EE >: E, T, T2](fa: IO[T])(f: T => IO[T2]): IO[T2] = fa.flatMap(f)


```

### stub

Generates a stub. Without expectations setup just throws **NotImplementedError**

```scala 3
import backstub.*

trait Foo:
  def zeroArgs: String
  def oneArg(x: Int): Int
  def moreArgs(x: Int, y: String): Option[String]

stub[Foo]
```

Will generate you:

```scala 3
new Foo:
  def zeroArgs: String = ???
  def oneArg(x: Int): Int = ???
  def moreArgs(x: Int, y: String): Option[String] = ???
```

### Expect

Compile time configuration allowing you to setup stub methods results.   
Expectation on method can be set only once, so if you want to differentiate calls - use different data.    
This also generates a collector for your calls.

```scala 3
import backstub.*

val foo = stub[Foo]:
  Expect[Foo]
    .method(_.oneArg).returns:
      case 1 => 2
      case 2 => 3
    .method(_.moreArgs).returns(_ => None)
```

Will generate you:

```scala 3
new Foo:
  def zeroArgs: String = ???
    
  val calls$oneArg$1 = new AtomicReference[List[Int]](Nil)
  
  def oneArg(x: Int): Int =
    calls$oneArg$1.getAndUpdate(_ :+ x)
    x match
      case 1 => 2
      case 2 => 3

  val calls$moreArgs$2 = new AtomicReference[List[(Int, String)]](Nil)
  
  def moreArgs(x: Int, y: String): Option[String] =
    calls$moreArgs$2.getAndUpdate(_ :+ (x, y))
    None

```

Also expectations can be provided via an inline given:

```scala 3
import backstub.*

inline given Expect[Foo] = Expect[Foo]
  .method(_.oneArg).returns:
    case 1 => 2
    case 2 => 3
  .method(_.moreArgs).returns(_ => None)


val foo = stub[Foo]
```

### ZIO Expect

The only difference is - you should choose method using `methodIO`.

### Cats Effect Expect

The only difference is - you should choose method using `methodF0` (if no arguments) or `methodF`.


### Verify

**backstub** won't verify anything for you, it only returns the data.

It gives you 2 extension methods for your stubs:

1. **calls** gives you the data
2. **times** gives you the number of times a method was called


```scala 3
import backstub.*

val foo = stub[Foo]

foo.oneArg(1)
foo.oneArg(2)

foo.times(_.oneArg) // 2
foo.calls(_.oneArg) // List(1, 2)

foo.twoArgs(5, "foo")

foo.times(_.twoArgs) // 1
foo.calls(_.twoArgs) // List((5, "foo"))

```

### Verify ZIO

You can use `callsIO` and `timesIO` methods

### Verify Cats Effect

You can use `callsF` and `timesF` methods

### Overloaded methods

To set expectations for overloaded methods - method type should be specified

```scala 3
import backstub.*

trait Overloaded:
  def overloaded(x: Int, y: Boolean): Int
  def overloaded(x: String): Boolean
  def overloaded: String

val overloaded = stub[Overloaded]:
  Expect[Overloaded]:
    .method(_.overloaded: String).returnsOnly("foo")
    .method(_.overloaded: String => Boolean).returns(_ => true)
    .method(_.overloaded: (Int, Boolean) => Int).returns((x, y) => x)
```

### Generic types support

Current implementation has restriction of 1 expectation per method.
It is same for generic types (Maybe it will change in the future if I find a solution).

```scala 3
import backstub.*

trait TypeArgs:
  def typeArgs[A](x: A): A

val typeArgsStub = stub[TypeArgs]:
  Expect[TypeArgs]:
    .method(_.typeArgs[Int]).returns(x => x)

typeArgsStub.typeArgs[Int](2)

typeArgsStub.times(_.typeArgs)
typeArgsStub.calls(_.typeArgs)

```


## Example
Model - [SessionCheckService.scala](modules/core/src/test/scala/backstub/SessionCheckService.scala)

Suite - [SessionCheckServiceSpec.scala](modules/core/src/test/scala/backstub/SessionCheckServiceSpec.scala)

## Search Optimization
scala tests, scala test, testing in scala, scala testing, scalatest, specs2, scalamock, zio mock, mockito scala, backstub, backstub scala












