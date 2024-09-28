package backstub

import scala.annotation.compileTimeOnly
import scala.util.{NotGiven, TupledFunction}

trait Expect[T]:

  def method[R](
      select: T => R
  )(using
      NotGiven[<:<[R, Tuple => ?]]
  ): Expect.Returns0[T, R]

  def method[Arg, R](select: T => Arg => R): Expect.Returns1[T, Arg, R]

  def method[F, Args <: ? *: ? *: EmptyTuple, R](
      select: T => F
  )(using
      TupledFunction[F, Args => R]
  ): Expect.Returns[T, Args, R]

object Expect:
  @compileTimeOnly("Expect[T] is considered to be an inline given or passed directly to stub[T]")
  def apply[T]: Expect[T] = ???

  trait Returns0[T, R]:
    def returnsOnly(value: R): Expect[T]

  trait Returns1[T, Arg, R]:
    def returns(value: Arg => R): Expect[T]

  trait Returns[T, Args <: Tuple, R]:
    def returns(value: Args => R): Expect[T]
