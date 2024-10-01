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

    def typeArgsOptIO[A](value: A): IO[Option[A]]

    def typeArgsOptIOTwoParams[A](value: A, other: A): IO[Option[A]]

  val foo: Stub[Foo] = stub[Foo]:
    Expect[Foo]
      .methodF0(_.zeroArgsIO).returnsOnly(IO(Some("foo")))
      .methodF(_.oneArgIO).returns(_ => IO(None))
      .methodF(_.twoArgsIO).returns((x, y) => IO(None))
      .methodF0(_.overloaded: IO[String]).returnsOnly(IO(""))
      .methodF(_.overloaded: String => IO[Boolean]).returns(x => IO(true))
      .methodF(_.overloaded: (Int, Boolean) => IO[Int]).returns((x, y) => IO(1))
      .methodF(_.typeArgsOptIO[String]).returns(x => IO.some(x))
      .methodF(_.typeArgsOptIOTwoParams[Int]).returns((x, y) => IO.some(x))

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


  test("type args one param"):
    val result = for
      result <- foo.typeArgsOptIO[String]("foo")
      times <- foo.timesF(_.typeArgsOptIO)
      calls <- foo.callsF(_.typeArgsOptIO)
    yield (result, times, calls)

    assertIO(result, (Some("foo"), 1, List("foo")))

  test("type args two params"):
    val result = for
      result <- foo.typeArgsOptIOTwoParams[Int](1, 2)
      times <- foo.timesF(_.typeArgsOptIOTwoParams)
      calls <- foo.callsF(_.typeArgsOptIOTwoParams)
    yield (result, times, calls)

    assertIO(result, (Some(1), 1, List((1, 2))))