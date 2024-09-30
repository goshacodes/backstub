package backstub

import effect.StubEffect

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

  def methodF0[F[+_]: StubEffect.Mono, R](
      select: T => F[R]
  ): Expect.Returns0[T, F[R]]

  def methodF[Arg, F[+_]: StubEffect.Mono, R](
      select: T => Arg => F[R]
  ): Expect.Returns1[T, Arg, F[R]]

  def methodF[Fun, Args <: ? *: ? *: EmptyTuple, F[+_]: StubEffect.Mono, R](
      select: T => Fun
  )(using
      TupledFunction[Fun, Args => F[R]]
  ): Expect.Returns[T, Args, F[R]]

  def methodIO[F[+_, +_]: StubEffect, E, R](
      select: T => F[E, R]
  ): Expect.Returns0[T, F[E, R]]

  def methodIO[Arg, F[+_, +_]: StubEffect, E, R](
      select: T => Arg => F[E, R]
  ): Expect.Returns1[T, Arg, F[E, R]]

  def methodIO[Fun, Args <: ? *: ? *: EmptyTuple, F[+_, +_]: StubEffect, E, R](
      select: T => Fun
  )(using
      TupledFunction[Fun, Args => F[E, R]]
  ): Expect.Returns[T, Args, F[E, R]]

object Expect:
  @compileTimeOnly("Expect[T] is considered to be an inline given or passed directly to stub[T]")
  def apply[T]: Expect[T] = throw IllegalAccessError()

  trait Returns0[T, R]:
    def returnsOnly[RR <: R](value: RR): Expect[T]

  trait Returns1[T, Arg, R]:
    def returns[RR <: R](value: Arg => RR): Expect[T]

  trait Returns[T, Args <: Tuple, R]:
    def returns[RR <: R](value: Args => RR): Expect[T]
