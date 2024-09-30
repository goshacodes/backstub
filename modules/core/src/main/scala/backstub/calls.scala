package backstub

import backstub.effect.StubEffect
import backstub.internal.UntupleOne

import scala.quoted.{Expr, Quotes, Type}
import scala.util.{NotGiven, TupledFunction}

extension [T](service: Stub[T])
  inline def calls[Fun, Args <: Tuple, R](
      inline select: T => Fun
  )(using
      TupledFunction[Fun, Args => R]
  ): List[UntupleOne[Args]] =
    ${ callsMacro[T, Fun, Args, R]('{ service }, '{ select }) }

  inline def callsF[F[+_]: StubEffect.Mono, Fun, Args <: Tuple, R](
      inline select: T => Fun
  )(using
      TupledFunction[Fun, Args => F[R]]
  ): F[List[UntupleOne[Args]]] =
    summon[StubEffect.Mono[F]]
      .unit(calls[Fun, Args, F[R]](select))

  inline def callsIO[F[+_, +_]: StubEffect, Fun, Args <: Tuple, E, R](
      inline select: T => Fun
  )(using
      TupledFunction[Fun, Args => F[E, R]]
  ): F[Nothing, List[UntupleOne[Args]]] =
    summon[StubEffect[F]].unit(calls[Fun, Args, F[E, R]](select))

  inline def times[F, Args <: Tuple, R](
      inline select: T => F
  ): Int =
    scala.compiletime.summonFrom {
      case given NotGiven[TupledFunction[F, _]] =>
        times0[F](select)

      case tf: TupledFunction[F, args => r] =>
        type Tupled[X] <: Tuple = X match
          case head *: tail => head *: tail

        calls[F, Tupled[args], r](select)(using
        tf.asInstanceOf).size
    }

  inline def timesF[Fun, Args <: Tuple, R, F[+_]: StubEffect.Mono](
      inline select: T => Fun
  ): F[Int] =
    scala.compiletime.summonFrom {
      case given NotGiven[TupledFunction[Fun, _]] =>
        summon[StubEffect.Mono[F]].unit(times0[Fun](select))

      case tf: TupledFunction[Fun, args => F[r]] =>
        type Tupled[X] <: Tuple = X match
          case head *: tail => head *: tail

        summon[StubEffect.Mono[F]].unit(
          calls[Fun, Tupled[args], F[r]](select)(using
          tf.asInstanceOf).size
        )
    }

  inline def timesIO[Fun, Args <: Tuple, R, F[+_, +_]: StubEffect](
      inline select: T => Fun
  ): F[Nothing, Int] =
    scala.compiletime.summonFrom {
      case given NotGiven[TupledFunction[Fun, _]] =>
        summon[StubEffect[F]].unit(times0[Fun](select))

      case tf: TupledFunction[Fun, args => F[e, r]] =>
        type Tupled[X] <: Tuple = X match
          case head *: tail => head *: tail

        summon[StubEffect[F]].unit(
          calls[Fun, Tupled[args], F[e, r]](select)(using
          tf.asInstanceOf).size
        )
    }

  inline private def times0[F](inline select: T => F): Int =
    ${ times0Macro[T, F]('{ service }, '{ select }) }

  inline private[backstub] def clear(): Unit =
    ${ clearMacro[T]('{ service }) }

private def times0Macro[T: Type, R: Type](
    service: Expr[T],
    select: Expr[T => R]
)(using
    quotes: Quotes
): Expr[Int] =
  new internal.Calls().times0[T, R](service, select)

private def callsMacro[T: Type, F: Type, Args <: Tuple: Type, R: Type](
    service: Expr[T],
    select: Expr[T => F]
)(using
    quotes: Quotes
): Expr[List[UntupleOne[Args]]] =
  new internal.Calls().calls[T, F, Args, R](service, select)

private def clearMacro[T: Type](
    service: Expr[T]
)(using
    quotes: Quotes
): Expr[Unit] =
  new internal.Calls().clearAll[T](service)
