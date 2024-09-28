package backstub.internal

import scala.quoted.*
import backstub.{CreatedStubs, Expect, Stub}

import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator
import scala.util.{NotGiven, TupledFunction}

private[backstub]
class StubCreation(using override val quotes: Quotes) extends StubUtils:
  import quotes.reflect.*

  def newInstance[T: Type](expectations: Expr[Expect[T]], collector: Expr[CreatedStubs]): Expr[T] =
    val tpe = TypeRepr.of[T]
    val parents = parentsOf[T]
    val methodsWithExpectations = parseMethodExpectations[T](expectations, methodsOf(tpe))

    val classSymbol = Symbol.newClass(
      parent = Symbol.spliceOwner,
      name = "anon",
      parents = parents.map(_.tpe),
      decls = classSymbol => methodsWithExpectations.flatMap { (method, expectation) =>
        List(
          Some(
            Symbol.newMethod(
              parent = classSymbol,
              name = method.symbol.name,
              tpe = method.symbol.info,
              flags = Flags.Override,
              privateWithin = Symbol.noSymbol
            )
          ),
          expectation.map { expectation =>
            val tpe = expectation.apply.tpe.asType match
              case '[args => res] =>
                TypeRepr.of[AtomicReference[List[args]]]
              case '[res] =>
                TypeRepr.of[AtomicReference[List[Unit]]]

            Symbol.newVal(
              parent = classSymbol,
              name = method.callsValName,
              tpe = tpe,
              flags = Flags.EmptyFlags,
              privateWithin = Symbol.noSymbol
            )
          }
        ).flatten
      },
      selfType = None
    )

    val classDef = ClassDef(
      cls = classSymbol,
      parents = parents,
      body = methodsWithExpectations.flatMap { (method, expectation) =>
        List(
          Some(
            DefDef(
              symbol = method.symbol.overridingSymbol(classSymbol),
              params => Some(expectation.fold('{???}.asTerm)(_.buildBody(classSymbol, method, params)))
            )
          ),
          expectation.map { expectation =>
            ValDef(
              classSymbol.declaredField(method.callsValName),
              Some {
                expectation.argsTpe.asType match
                  case '[args] =>
                    '{ new AtomicReference(List.empty[args]) }.asTerm
              }
            )
          }
        ).flatten
      }
    )

    val instance = Block(
      List(classDef),
      Typed(
        Apply(
          Select(New(TypeIdent(classSymbol)), classSymbol.primaryConstructor),
          Nil
        ),
        TypeTree.of[T & scala.reflect.Selectable]
      )
    )

    '{
      ${collector}.bind(${instance.asExprOf[T]}.asInstanceOf[Stub[T]])
    }

  case class Expectation(apply: Term):
    val argsTpe = apply.tpe.asType match
      case '[args => res] =>
        TypeRepr.of[args]
      case '[res] =>
        TypeRepr.of[Unit]

    def buildBody(classSymbol: Symbol, method: Method, params: List[List[Tree | Term]]) =
      params match
        case List(params) =>
          apply.tpe.asType match
            case '[argsType => resType] =>
              val calls = Ref(classSymbol.declaredField(method.callsValName)).asExprOf[AtomicReference[List[argsType]]]
              val tupledArgs = params.collect { case term: Term => term } match
                case Nil => report.errorAndAbort("Unexpected error occurred, please open an issue")
                case arg :: Nil => arg
                case args => tupled(args)
              '{
                ${ calls }.getAndUpdate(_ :+ ${ tupledArgs.asExprOf[argsType] })
                ${ Select.unique(apply, "apply").appliedTo(tupledArgs).asExprOf[resType] }
              }.asTerm.changeOwner(method.symbol.overridingSymbol(classSymbol))


        case Nil =>
          apply.tpe.asType match
            case '[resType] =>
              val calls = Ref(classSymbol.declaredField(method.callsValName)).asExprOf[AtomicReference[List[Unit]]]
              '{
                ${ calls }.getAndUpdate(_ :+ ())
                ${ apply.asExprOf[resType] }
              }.asTerm.changeOwner(method.symbol.overridingSymbol(classSymbol))


        case _ =>
          report.errorAndAbort(s"Unknown param clause ${params.map(_.map(_.show(using Printer.TreeStructure)))}")


  private def parentsOf[T: Type]: List[TypeTree] =
    val tpe = TypeRepr.of[T]
    val sym = tpe.dealias.typeSymbol
    val isTrait = sym.flags.is(Flags.Trait)

    def noParams = sym.primaryConstructor.paramSymss.flatten.filterNot(_.isType).isEmpty

    val deferedVals = tpe.typeSymbol.fieldMembers.filter(_.flags.is(Flags.Deferred))

    if !(isTrait && noParams) then
      report.errorAndAbort("Only traits without parameters are allowed")
    else if deferedVals.nonEmpty then
      report.errorAndAbort("Interface shouldn't contain deferred vals")
    else if isTrait then
      List(TypeTree.of[Object], TypeTree.of[T], TypeTree.of[scala.reflect.Selectable])
    else
      List(TypeTree.of[T], TypeTree.of[scala.reflect.Selectable])

  private def parseMethodExpectations[T: Type](
    expectations: Expr[Expect[T]],
    methods: List[Method]
  ): List[(Method, Option[Expectation])] =

    @scala.annotation.tailrec
    def loop(conf: Expr[Expect[T]], expectations: List[(Method, Expectation)]): List[(Method, Expectation)] =
      conf match
        case '{ Expect[T] } => expectations

        case '{
          (${rest}: Expect[T])
            .method[res](${select}: T => res)(using ${_}: NotGiven[<:<[res, Tuple => ?]])
            .returnsOnly(${value}: res)
        } =>
          val method = methods.searchMethod(select.asTerm, None, TypeRepr.of[res])

          val expectation = Expectation(value.asTerm)
          loop(rest, (method -> expectation) :: expectations)

        case '{
          (${rest}: Expect[T])
            .method[arg, res](${select}: T => arg => res)
            .returns(${apply}: arg => res)
        } =>
          val method = methods.searchMethod(select.asTerm, Some(TypeRepr.of[Tuple1[arg]]), TypeRepr.of[res])

          val expectation = Expectation(apply.asTerm)
          loop(rest, (method -> expectation) :: expectations)


        case '{
          type args <: ? *: ? *: EmptyTuple
          (${rest}: Expect[T])
            .method[f, args, res]((${select}: T => f))(using ${_}: TupledFunction[f, args => res])
            .returns((${apply}: args => res))
        } =>
          val method = methods.searchMethod(select.asTerm, Some(TypeRepr.of[args]), TypeRepr.of[res])
          val expectation = Expectation(apply.asTerm)
          loop(rest, (method -> expectation) :: expectations)

        case expr =>
          report.errorAndAbort(s"Unknown tree: ${expr.asTerm.show(using Printer.TreeShortCode)}")


    val methodsWithExpectations = loop(expectations, Nil)
    val methodsWithoutExpectations = methods.filterNot(method => methodsWithExpectations.exists(_._1 == method))

    methodsWithExpectations.map((method, expectation) => (method, Option(expectation))) ++ methodsWithoutExpectations.map(_ -> None)


