package backstub

import scala.quoted.*
import scala.util.{NotGiven, TupledFunction}

type Stub[T] = stub.Stub[T]

object stub:
  opaque type Stub[+T] <: T = T

  inline def apply[T](using
      collector: CreatedStubs
  ): Stub[T] =
    ${ stubMacro[T]('{ collector }) }

  inline def apply[T](
      inline expect: Expect[T]
  )(using
      collector: CreatedStubs
  ): Stub[T] =
    ${ stubMacro[T]('{ expect }, '{ collector }) }

  private def stubMacro[T: Type](
      expect: Expr[Expect[T]],
      collector: Expr[CreatedStubs]
  )(using
      quotes: Quotes): Expr[T] =
    new internal.StubCreation().newInstance[T](expect, collector)

  private def stubMacro[T: Type](
      collector: Expr[CreatedStubs]
  )(using
      quotes: Quotes): Expr[T] =
    new internal.StubCreation().newInstance[T](Expr.summon[Expect[T]].getOrElse('{ Expect[T] }), collector)

extension [T](service: Stub[T])
  inline def calls[F, Args <: Tuple, R](inline select: T => F)(using
      TupledFunction[F, Args => R]): List[Args] =
    ${ callsMacro[T, F, Args, R]('{ service }, '{ select }) }

  inline def times[F, Args <: Tuple, R](inline select: T => F): Int =
    scala.compiletime.summonFrom {
      case given NotGiven[TupledFunction[F, _]] =>
        times0[F](select)

      case tf: TupledFunction[F, args => r] =>
        type Tupled[X] <: Tuple = X match
          case head *: tail => head *: tail

        calls[F, Tupled[args], r](select)(using
        tf.asInstanceOf).size
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
): Expr[List[Args]] =
  new internal.Calls().calls[T, F, Args, R](service, select)

private def clearMacro[T: Type](
    service: Expr[T]
)(using
    quotes: Quotes
): Expr[Unit] =
  new internal.Calls().clearAll[T](service)
