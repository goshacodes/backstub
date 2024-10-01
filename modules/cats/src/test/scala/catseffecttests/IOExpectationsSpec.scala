package catseffecttests

import backstub.*
import cats.effect.IO
import munit.CatsEffectSuite

class IOExpectationsSpec extends CatsEffectSuite, CatsEffectStubs:
  override def beforeEach(context: BeforeEach) =
    resetStubs()

  trait Foo:
    def zeroArgsIO: IO[Option[String]]

    def oneArgIO(x: Int): IO[Option[String]]

    def twoArgsIO(x: Int, y: String): IO[Option[String]]

    def overloaded(x: Int, y: Boolean): IO[Int]

    def overloaded(x: String): IO[Boolean]

    def overloaded: IO[String]
    
    def typeArgs[A, B](x: A): B

  val foo: Stub[Foo] = stub[Foo]:
    Expect[Foo]
      .methodF0(_.zeroArgsIO).returnsOnly(IO(Some("foo")))
      .methodF(_.oneArgIO).returns(_ => IO(None))
      .methodF(_.twoArgsIO).returns((x, y) => IO(None))
      .methodF0(_.overloaded: IO[String]).returnsOnly(IO(""))
      .methodF(_.overloaded: String => IO[Boolean]).returns(x => IO(true))
      .methodF(_.overloaded: (Int, Boolean) => IO[Int]).returns((x, y) => IO(1))

  test("zero args"):
    val result = for
      _ <- foo.zeroArgsIO
      _ <- foo.zeroArgsIO
      times <- foo.timesF(_.zeroArgsIO)
    yield times

    assertIO(result, 2)

  test("one arg"):
    val result = for
      _ <- foo.oneArgIO(1)
      _ <- foo.oneArgIO(2)
      times <- foo.timesF(_.oneArgIO)
      calls <- foo.callsF(_.oneArgIO)
    yield (times, calls)

    assertIO(result, (2, List(1, 2)))


  test("two args"):
    val result = for
      _ <- foo.twoArgsIO(1, "foo")
      _ <- foo.twoArgsIO(2, "bar")
      times <- foo.timesF(_.twoArgsIO)
      calls <- foo.callsF(_.twoArgsIO)
    yield (times, calls)

    assertIO(result, (2, List((1, "foo"), (2, "bar"))))
