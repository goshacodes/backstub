package ziotests

import backstub.*
import zio.*
import zio.test.*

object ZIOExpectationsSpec extends ZIOSpecDefault, ZIOStubs:

  trait Foo:
    def zeroArgsTask: Task[Option[String]]
    def zeroArgsTaskFail: Task[Option[String]]
    def zeroArgsUIO: UIO[Option[String]]
    def zeroArgsIO: IO[Int, Option[String]]
    def zeroArgsIOFail: IO[Int, Option[String]]

    def oneArgTask(x: Int): Task[Option[String]]
    def oneArgTaskFail(x: Int): Task[Option[String]]
    def oneArgUIO(x: Int): UIO[Option[String]]
    def oneArgIO(x: Int): IO[Int, Option[String]]
    def oneArgIOFail(x: Int): IO[Int, Option[String]]

    def twoArgsTask(x: Int, y: String): Task[Option[String]]
    def twoArgsTaskFail(x: Int, y: String): Task[Option[String]]
    def twoArgsUIO(x: Int, y: String): UIO[Option[String]]
    def twoArgsIO(x: Int, y: String): IO[Int, Option[String]]
    def twoArgsIOFail(x: Int, y: String): IO[Int, Option[String]]

    def overloaded(x: Int, y: Boolean): UIO[Int]

    def overloaded(x: String): UIO[Boolean]

    def overloaded: UIO[String]

    def typeArgsOptUIO[A](value: A): UIO[Option[A]]

    def typeArgsOptUIOTwoParams[A](value: A, other: A): Task[Option[A]]

  val foo: Stub[Foo] = stub[Foo]:
    Expect[Foo]
      .methodIO(_.zeroArgsUIO).returnsOnly(ZIO.none)
      .methodIO(_.zeroArgsTask).returnsOnly(ZIO.some("foo"))
      .methodIO(_.zeroArgsTaskFail).returnsOnly(ZIO.fail(new RuntimeException("foo")))
      .methodIO(_.zeroArgsIO).returnsOnly(ZIO.none)
      .methodIO(_.zeroArgsIOFail).returnsOnly(ZIO.fail(2))
      .methodIO(_.oneArgUIO).returns(_ => ZIO.none)
      .methodIO(_.oneArgTask).returns(_ => ZIO.some("foo"))
      .methodIO(_.oneArgTaskFail).returns(_ => ZIO.fail(new RuntimeException("foo")))
      .methodIO(_.oneArgIO).returns(_ => ZIO.none)
      .methodIO(_.oneArgIOFail).returns(_ => ZIO.fail(2))
      .methodIO(_.twoArgsUIO).returns((x, y) => ZIO.none)
      .methodIO(_.twoArgsTask).returns((x, y) => ZIO.some("foo"))
      .methodIO(_.twoArgsTaskFail).returns((x, y) => ZIO.fail(new RuntimeException("foo")))
      .methodIO(_.twoArgsIO).returns((x, y) => ZIO.none)
      .methodIO(_.twoArgsIOFail).returns((x, y) => ZIO.fail(2))
      .methodIO(_.overloaded: UIO[String]).returnsOnly(ZIO.succeed(""))
      .methodIO(_.overloaded: String => UIO[Boolean]).returns(x => ZIO.succeed(true))
      .methodIO(_.overloaded: (Int, Boolean) => UIO[Int]).returns((x, y) => ZIO.succeed(1))
      .methodIO(_.typeArgsOptUIO[String]).returns(x => ZIO.some(x))
      .methodIO(_.typeArgsOptUIOTwoParams[Int]).returns((x, y) => ZIO.some(x))

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("check expectations with zio")(
      test("zero args"):
        for
          _ <- foo.zeroArgsTask.repeatN(10)
          times <- foo.timesIO(_.zeroArgsTask)
          result = assertTrue(times == 11)
        yield result,
      test("two args and cleanup"):
        for
          _ <- foo.twoArgsIOFail(1, "").orElse(ZIO.none)
          calls <- foo.callsIO(_.twoArgsIOFail)
          times2 <- foo.timesIO(_.zeroArgsTask)
          result = assertTrue(calls == List((1, "")), times2 == 0)
        yield result,
      test("one arg"):
        for
          _ <- foo.oneArgIO(1)
          times <- foo.timesIO(_.oneArgIO)
          calls <- foo.callsIO(_.oneArgIO)
          result = assertTrue(times == 1, calls == List(1))
        yield result,
      test("type args one param"):
        for
          result <- foo.typeArgsOptUIO[String]("foo")
          times <- foo.timesIO(_.typeArgsOptUIO)
          calls <- foo.callsIO(_.typeArgsOptUIO)
        yield assertTrue(result.contains("foo"), times == 1, calls == List("foo")),
      test("type args two params"):
        for
          result <- foo.typeArgsOptUIOTwoParams[Int](1, 2).repeatN(1)
          times <- foo.timesIO(_.typeArgsOptUIOTwoParams)
          calls <- foo.callsIO(_.typeArgsOptUIOTwoParams)
        yield assertTrue(result.contains(1), times == 2, calls == List((1, 2), (1, 2)))
    ) @@ TestAspect.before(resetStubsIO) @@ TestAspect.sequential






