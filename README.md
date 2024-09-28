![GitHub Release](https://img.shields.io/github/v/release/goshacodes/backstub?color=blue])

## Backstub

1. Tired of choosing between mocks and stubs?
2. Tired of argument matchers and thrown exceptions when expectations not met?
3. Or you just don't like 'mocks/stubs' approach entirely?

Then you've come to the right place, meet **backstub** - stubbing library for Scala 3 inspired by [scalamock](https://github.com/paulbutcher/ScalaMock) and compatible with any testing framework out of the box

How it works:
1. Setup cleanup after each test-case
2. Setup expected results based on input arguments per suit or per test-case
3. After stubs were used - get the data passed through the method and verify

## Setup

Add to your build.sbt

```scala
libraryDependencies += "io.github.goshacodes" %% "backstub" % "<version_from_badge>"
```

Library hugely relies on experimental scala 3 features, so consider also adding

```scala
Test \ scalacOptions += "-experimental"
```

## API

### Stubs

Should be mixed with your test-suite, to provide clean-up API

```scala 3
package backstub

trait Stubs:
  given stubs: CreatedStubs = CreatedStubs()

  def resetStubs(): Unit = stubs.clearAll()

```

Using it as simple as:

```scala 3
import backstub.*

class MySpec extends munit.FunSuite with Stubs:
  override def afterEach(context: AfterEach) =
    resetStubs()
    
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

## Example
Model - [SessionCheckService.scala](./src/test/scala/backstub/SessionCheckService.scala)

Suite - [SessionCheckServiceSpec.scala](./src/test/scala/backstub/SessionCheckServiceSpec.scala)

## Notes
Only basic functionality is supported by now.  

It won't block your stub generation, but you won't set up an expectation:
1. Methods with type parameters are not supported
2. Curried methods are not supported
3. Probably something else is not supported, you can always open an issue to discuss and a PR

## Gratitudes
If this library makes you so incredibly happy, that you really want to donate - contact me using link in github profile

## Search Optimization
scala tests, scala test, testing in scala, scala testing, scalatest, specs2, scalamock, zio mock, mockito scala












