package backstub

import backstub.*

class ExpectationsSpec extends munit.FunSuite with Stubs {

  override def afterEach(context: AfterEach) =
    resetStubs()


  trait Foo:
    def twoArgs(x: Int, y: String): Option[String]
    def oneArg(x: Int): Int
    def zeroArgs: String
    def overloaded(x: Int, y: Boolean): Int
    def overloaded(x: String): Boolean
    def overloaded: String


  test("zero args"):
    val foo = stub[Foo]:
      Expect[Foo]
        .method[String](_.zeroArgs).returnsOnly("5")

    val res = foo.zeroArgs
    foo.zeroArgs
    foo.zeroArgs

    val callsCount = foo.times(_.zeroArgs)

    assert(res == "5")
    assert(callsCount == 3)

  test("one arg"):
    val foo = stub[Foo]:
      Expect[Foo]
        .method(_.oneArg).returns {
          case 1 => 2
          case _ => 3
        }

    val res = foo.oneArg(1)
    val res2 = foo.oneArg(0)

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

}
