package backstub

import scala.quoted.*

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