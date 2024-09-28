package backstub.internal

import java.util.concurrent.atomic.AtomicReference
import scala.quoted.*

private[backstub]
class Calls(using override val quotes: Quotes) extends StubUtils:
  import quotes.reflect.*

  def times0[T: Type, R: Type](
    stub: Expr[T],
    select: Expr[T => R]
  ): Expr[Int] =
    val method = methodsOf(TypeRepr.of[T]).searchMethod(select.asTerm, None, TypeRepr.of[R])
    '{
      ${ stub }
        .asInstanceOf[scala.reflect.Selectable]
        .selectDynamic(${ Expr(method.callsValName) })
        .asInstanceOf[AtomicReference[List[Unit]]]
        .get()
        .length
    }

  def calls[T: Type, F: Type, Args <: Tuple: Type, R: Type](
    stub: Expr[T],
    select: Expr[T => F]
  ): Expr[List[Args]] =
    val method = methodsOf(TypeRepr.of[T]).searchMethod(select.asTerm, Some(TypeRepr.of[Args]), TypeRepr.of[R])
    '{
      ${ stub }
        .asInstanceOf[scala.reflect.Selectable]
        .selectDynamic(${ Expr(method.callsValName) })
        .asInstanceOf[AtomicReference[List[Args]]]
        .get()
    }


  def clearAll[T: Type](stub: Expr[T]): Expr[Unit] =
    Expr.block(methodsOf(TypeRepr.of[T]).map(clearOne[T](stub, _)), '{})

  private def clearOne[T: Type](stub: Expr[T], method: Method): Expr[Unit] =
    '{
      try ${ stub }
        .asInstanceOf[scala.reflect.Selectable]
        .selectDynamic(${ Expr(method.callsValName) })
        .asInstanceOf[AtomicReference[List[?]]]
        .set(Nil)
      catch
        case ex: NoSuchMethodException =>
    }
