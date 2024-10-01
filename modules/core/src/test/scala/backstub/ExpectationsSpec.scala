package backstub

import backstub.*

class ExpectationsSpec extends munit.FunSuite, Stubs:

  override def afterEach(context: AfterEach) =
    resetStubs()

  trait Overlap:
    def overlap(x: Int): scala.Predef.String

  val overlap = stub[Overlap]:
    Expect[Overlap]
      .method(_.overlap).returns(_ => "foo")

  trait Foo:
    def twoArgs(x: Int, y: String): Option[String]
    def oneArg(x: Int): Int
    def zeroArgs: String
    def overloaded(x: Int, y: Boolean): Int
    def overloaded(x: String): Boolean
    def overloaded: String
    def curried(x: Int)(y: String): String
    def otherCurried(x: Int, y: String)(z: Boolean, foo: Long): Int
    def oneMoreCurried(x: Int)(y: String)(z: Boolean, foo: Long): Int


  test("zero args"):
    val foo = stub[Foo]:
      Expect[Foo]
        .method(_.zeroArgs).returnsOnly("5")

    val res = foo.zeroArgs
    foo.zeroArgs
    foo.zeroArgs

    val callsCount = foo.times(_.zeroArgs)

    assert(res == "5")
    assert(callsCount == 3)

    overlap.overlap(1)


  test("one arg"):
    val foo = stub[Foo]:
      Expect[Foo]
        .method(_.oneArg).returns {
          case 1 => 2
          case _ => 3
        }

    val res = foo.oneArg(1)
    val res2 = foo.oneArg(0)

    assert(overlap.times(_.overlap) == 0)
    assert(res == 2)
    assert(res2 == 3)
    assert(foo.calls(_.oneArg) == List(1, 0))
    assert(foo.times(_.oneArg) == 2)

  test("two args"):
    val foo = stub[Foo]:
      Expect[Foo]
        .method(_.twoArgs).returns {
          case (1, "2") => Some("foo")
          case _ => None
        }

    val res = foo.twoArgs(1, "2")
    val res2 = foo.twoArgs(0, "2")

    assert(res.contains("foo"))
    assert(res2.isEmpty)
    assert(foo.calls(_.twoArgs) == List((1, "2"), (0, "2")))
    assert(foo.times(_.twoArgs) == 2)


  test("overloaded"):
    val foo = stub[Foo]:
      Expect[Foo]
        .method(_.overloaded: String => Boolean).returns(_ => true)
        .method(_.overloaded: (Int, Boolean) => Int).returns((x, y) => x)
        .method(_.overloaded: String).returnsOnly("foo")

    foo.overloaded("bar")
    foo.overloaded(1, true)
    foo.overloaded

    val firstCalls = foo.calls(_.overloaded: String => Boolean)
    val secondCalls = foo.calls(_.overloaded: (Int, Boolean) => Int)
    val thirdCallsTimes = foo.times(_.overloaded: String)

    assert(firstCalls == List("bar"))
    assert(secondCalls == List((1, true)))
    assert(thirdCallsTimes == 1)

  test("curried"):
    val foo = stub[Foo]:
      Expect[Foo]
        .method(_.curried).returns(x => y => "")
        .method(_.otherCurried).returns((a, b) => (c, d) => 5)
        .method(_.oneMoreCurried).returns(a => b => (c, d) => 6)

    foo.curried(1)("2")
    foo.curried(2)("3")
    foo.otherCurried(1, "2")(true, 5)
    foo.otherCurried(2, "3")(false, 6)

    foo.oneMoreCurried(1)("2")(false, 5)
    foo.oneMoreCurried(2)("3")(true, 6)

    val firstCalls = foo.calls(_.curried)
    val secondCalls = foo.calls(_.otherCurried)
    val thirdCalls = foo.calls(_.oneMoreCurried)
    assert(firstCalls == List((1, "2"), (2, "3")))
    assert(secondCalls == List(((1, "2"), (true, 5)), ((2, "3"), (false, 6))))
    assert(thirdCalls == List((1, "2", (false, 5)), (2, "3", (true, 6))))
