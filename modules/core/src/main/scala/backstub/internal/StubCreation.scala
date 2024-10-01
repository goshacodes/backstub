package backstub.internal

import backstub.effect.StubEffect
import backstub.{CreatedStubs, Expect, Stub, effect}

import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator
import scala.quoted.*
import scala.util.{NotGiven, TupledFunction}

private[backstub] class StubCreation(using
    Quotes
) extends StubUtils:
  import quotes.reflect.*

  def newInstance[T: Type](expectations: Expr[Expect[T]], collector: Expr[CreatedStubs]): Expr[T] =
    val tpe = TypeRepr.of[T]
    val parents = parentsOf[T]
    val methodsWithExpectations = parseMethodExpectations[T](expectations, methodsOf(tpe))

    val classSymbol = Symbol.newClass(
      parent = Symbol.spliceOwner,
      name = "anon",
      parents = parents.map(_.tpe),
      decls = classSymbol =>
        methodsWithExpectations.flatMap { (method, expectation) =>
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
              Symbol.newVal(
                parent = classSymbol,
                name = method.callsValName,
                tpe = TypeRepr.of[AtomicReference[List[Any]]],
                flags = Flags.EmptyFlags,
                privateWithin = Symbol.noSymbol
              )
            }
          ).flatten
        } :+ Symbol.newMethod(
          parent = classSymbol,
          name = clearStubsMethodName,
          tpe = TypeRepr.of[Unit],
          flags = Flags.EmptyFlags,
          privateWithin = Symbol.noSymbol
        ),
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
              params => Some(expectation.fold('{ ??? }.asTerm)(_.buildBody(classSymbol, method, params)))
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
      } :+ DefDef(
        symbol = classSymbol.methodMember(clearStubsMethodName).head,
        _ =>
          Some(
            Block(
              methodsWithExpectations
                .collect { case (method, Some(_)) => classSymbol.declaredField(method.callsValName) }
                .map { symbol =>
                  Apply(Select.unique(Ref(symbol), "set"), List('{ Nil }.asTerm))
                },
              '{}.asTerm
            )
          )
      )
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
      ${ collector }.bind(${ instance.asExprOf[T] }.asInstanceOf[Stub[T]])
    }

  private case class Expectation(apply: Term, monad: Option[(Term, TypeTree, TypeTree)]):
    val argsTpe = TypeRepr.of[Any]

    def buildBody(classSymbol: Symbol, method: Method, params: List[List[Tree | Term]]) =
      val args = params.map { _.collect { case term: Term => term } }.filterNot(_.isEmpty)
      val calls = Ref(classSymbol.declaredField(method.callsValName)).asExprOf[AtomicReference[List[Any]]]
      val (argsToUpdate, result) = args match
        case Nil =>
          ('{ () }.asTerm, apply)
        case params =>
          def listTupled(args: List[Term]) = args match
            case Nil        => report.errorAndAbort("Unexpected error occurred, please open an issue")
            case arg :: Nil => arg
            case args       => tupled(args)

          val result = (List(listTupled(args.head)) :: args.tail)
            .foldLeft(apply) { (applied, args) => Apply(Select.unique(applied, "apply"), args.map(Select.unique(_, "asInstanceOf"))) }
          
          (listTupled(args.map(listTupled)), result)

      val updateCalls = '{ ${ calls }.getAndUpdate(_ :+ ${ argsToUpdate.asExprOf[Any] }) }

      val body = method.resolveParamRefs(params).asType match
        case '[res] =>
          monad match
            case None =>
              '{
                ${ updateCalls }
                ${ result.asExprOf[Any] }.asInstanceOf[res]
              }.asTerm
            case Some((monad, errorTpt, tpt)) =>
              result.tpe.asType match
                case '[effResType] =>
                  TypeApply(
                    Select.unique(
                      Apply(
                        Apply(
                          TypeApply(
                            Select.unique(monad, "flatMap"),
                            List(TypeTree.of[Nothing], errorTpt, TypeTree.of[Unit], tpt)
                          ),
                          List(
                            Apply(
                              TypeApply(Select.unique(monad, "unit"), List(TypeTree.of[Unit])),
                              List(updateCalls.asTerm)
                            )
                          )
                        ),
                        List('{ (_: Unit) => ${ result.asExprOf[effResType] } }.asTerm)
                      ),
                      "asInstanceOf"
                    ),
                    List(TypeTree.of[res])
                  )
                  


      body.changeOwner(method.symbol.overridingSymbol(classSymbol))

    end buildBody

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
    def fixedErrorTpt(tpt: TypeTree) =
      if tpt.tpe == TypeRepr.of[Any] then TypeTree.of[Nothing] else tpt

    @scala.annotation.tailrec
    def loop(conf: Expr[Expect[T]], expectations: List[(Method, Expectation)]): List[(Method, Expectation)] =
      conf match
        case '{ Expect[T] } => expectations

        case '{
              (${ rest }: Expect[T])
                .method[res](${ select }: T => res)(using
                ${ _ }: NotGiven[<:<[res, Tuple => ?]])
                .returnsOnly(${ value }: res)
            } =>
          val method = methods.searchMethod(select.asTerm, None, TypeRepr.of[res])

          if (expectations.exists(_._1.symbol == method.symbol))
            report.errorAndAbort(s"Expectation for ${select.asTerm.show(using
              Printer.TreeShortCode)} is already set")

          val expectation = Expectation(value.asTerm, None)
          loop(rest, (method -> expectation) :: expectations)

        case '{
              type effect[+r];

              (${ rest }: Expect[T])
                .methodF0[effect, r](${ select }: T => effect[r])(using
                ${ monad }: StubEffect.Mono[effect])
                .returnsOnly[effect[r]](${ value }: effect[r])
            } =>
          val method = methods.searchMethod(select.asTerm, None, TypeRepr.of[effect[r]])

          if (expectations.exists(_._1.symbol == method.symbol))
            report.errorAndAbort(s"Expectation for ${select.asTerm.show(using
              Printer.TreeShortCode)} is already set")

          val expectation = Expectation(value.asTerm, Some((monad.asTerm, TypeTree.of[Nothing], TypeTree.of[r])))
          loop(rest, (method -> expectation) :: expectations)

        case '{
              type effect[+e, +r];

              (${ rest }: Expect[T])
                .methodIO[effect, e, r](${ select }: T => effect[e, r])(using
                ${ monad }: StubEffect[effect])
                .returnsOnly[effect[e, r]](${ value }: effect[e, r])
            } =>
          val fixedError = fixedErrorTpt(TypeTree.of[e])

          fixedError.tpe.asType match
            case '[fe] =>
              val method = methods.searchMethod(select.asTerm, None, TypeRepr.of[effect[fe, r]])

              if (expectations.exists(_._1.symbol == method.symbol))
                report.errorAndAbort(s"Expectation for ${select.asTerm.show(using
                  Printer.TreeShortCode)} is already set")

              val expectation = Expectation(value.asTerm, Some((monad.asTerm, fixedError, TypeTree.of[r])))
              loop(rest, (method -> expectation) :: expectations)

        case '{
              (${ rest }: Expect[T])
                .method[arg, res](${ select }: T => arg => res)
                .returns(${ apply }: arg => res)
            } =>
          val method = methods.searchMethod(select.asTerm, Some(TypeRepr.of[Tuple1[arg]]), TypeRepr.of[res])

          if (expectations.exists(_._1.symbol == method.symbol))
            report.errorAndAbort(s"Expectation for ${select.asTerm.show(using
              Printer.TreeShortCode)} is already set")

          val expectation = Expectation(apply.asTerm, None)
          loop(rest, (method -> expectation) :: expectations)

        case '{
              type effect[+r];

              (${ rest }: Expect[T])
                .methodF[arg, effect, r](${ select }: T => arg => effect[r])(using
                ${ monad }: StubEffect.Mono[effect])
                .returns[effect[r]](${ value }: arg => effect[r])
            } =>
          val method = methods.searchMethod(select.asTerm, Some(TypeRepr.of[Tuple1[arg]]), TypeRepr.of[effect[r]])

          if (expectations.exists(_._1.symbol == method.symbol))
            report.errorAndAbort(s"Expectation for ${select.asTerm.show(using
              Printer.TreeShortCode)} is already set")

          val expectation = Expectation(value.asTerm, Some((monad.asTerm, TypeTree.of[Nothing], TypeTree.of[r])))
          loop(rest, (method -> expectation) :: expectations)

        case '{
              type effect[+e, +r];

              (${ rest }: Expect[T])
                .methodIO[arg, effect, e, r](${ select }: T => arg => effect[e, r])(using
                ${ monad }: StubEffect[effect])
                .returns[effect[e, r]](${ value }: arg => effect[e, r])
            } =>
          val fixedError = fixedErrorTpt(TypeTree.of[e])

          fixedError.tpe.asType match
            case '[fe] =>
              val method =
                methods.searchMethod(select.asTerm, Some(TypeRepr.of[Tuple1[arg]]), TypeRepr.of[effect[fe, r]])

              if (expectations.exists(_._1.symbol == method.symbol))
                report.errorAndAbort(s"Expectation for ${select.asTerm.show(using
                  Printer.TreeShortCode)} is already set")

              val expectation = Expectation(value.asTerm, Some((monad.asTerm, fixedError, TypeTree.of[r])))
              loop(rest, (method -> expectation) :: expectations)

        case '{
              type args <: ? *: ? *: EmptyTuple;
              (${ rest }: Expect[T])
                .method(${ select }: T => f)(using
                ${ _ }: TupledFunction[f, args => res])
                .returns(${ apply }: args => res)
            } =>
          val method = methods.searchMethod(select.asTerm, Some(TypeRepr.of[args]), TypeRepr.of[res])

          if (expectations.exists(_._1.symbol == method.symbol))
            report.errorAndAbort(s"Expectation for ${select.asTerm.show(using
              Printer.TreeShortCode)} is already set")

          val expectation = Expectation(apply.asTerm, None)
          loop(rest, (method -> expectation) :: expectations)

        case '{
              type args <: ? *: ? *: EmptyTuple;
              type effect[+r];

              (${ rest }: Expect[T])
                .methodF[f, args, effect, r](${ select }: T => f)(using
                  ${ monad }: StubEffect.Mono[effect],
                  ${ _ }: TupledFunction[f, args => effect[r]]
                )
                .returns[effect[r]](${ value }: args => effect[r])
            } =>
          val method = methods.searchMethod(select.asTerm, Some(TypeRepr.of[args]), TypeRepr.of[effect[r]])

          if (expectations.exists(_._1.symbol == method.symbol))
            report.errorAndAbort(s"Expectation for ${select.asTerm.show(using
              Printer.TreeShortCode)} is already set")

          val expectation = Expectation(value.asTerm, Some((monad.asTerm, TypeTree.of[Nothing], TypeTree.of[r])))
          loop(rest, (method -> expectation) :: expectations)

        case '{
              type args <: ? *: ? *: EmptyTuple;
              type effect[+e, +r];

              (${ rest }: Expect[T])
                .methodIO[f, args, effect, e, r](${ select }: T => f)(using
                  ${ monad }: StubEffect[effect],
                  ${ _ }: TupledFunction[f, args => effect[e, r]]
                )
                .returns(${ value }: args => effect[e, r])
            } =>
          val fixedError = fixedErrorTpt(TypeTree.of[e])

          fixedError.tpe.asType match
            case '[fe] =>
              val method = methods.searchMethod(select.asTerm, Some(TypeRepr.of[args]), TypeRepr.of[effect[fe, r]])

              if (expectations.exists(_._1.symbol == method.symbol))
                report.errorAndAbort(s"Expectation for ${select.asTerm.show(using
                  Printer.TreeShortCode)} is already set")

              val expectation = Expectation(value.asTerm, Some((monad.asTerm, fixedError, TypeTree.of[r])))
              loop(rest, (method -> expectation) :: expectations)

        case expr =>
          expr.asTerm match
            case MethodIO(rest, select, apply, monad, effectTpt, errorTpt, resultTpt, argsTpt) =>
              val method = methods.searchMethod(
                select,
                Some(argsTpt.tpe),
                effectTpt.tpe.appliedTo(List(errorTpt.tpe, resultTpt.tpe))
              )

              val expectation = Expectation(apply, Some((monad, errorTpt, resultTpt)))
              loop(rest.asExprOf[Expect[T]], (method -> expectation) :: expectations)
            case _ =>
              report.errorAndAbort(s"Unknown tree: ${expr.asTerm.show(using
                Printer.TreeShortCode)}")

    val methodsWithExpectations = loop(expectations, Nil)
    val methodsWithoutExpectations = methods.filterNot(method => methodsWithExpectations.exists(_._1 == method))

    methodsWithExpectations.map((method, expectation) =>
      (method, Option(expectation))
    ) ++ methodsWithoutExpectations.map(_ -> None)

  object MethodIO:
    private def fixedErrorTpt(tpt: TypeTree) =
      if tpt.tpe == TypeRepr.of[Any] then TypeTree.of[Nothing] else tpt

    def unapply(term: Term): Option[(Term, Term, Term, Term, TypeTree, TypeTree, TypeTree, TypeTree)] = term match
      case Inlined(_, _, term) => unapply(term)
      case Apply(
            TypeApply(
              Select(
                Apply(
                  Apply(
                    TypeApply(Select(rest, "methodIO"), List(funTpt, argsTpt, effTpt, errorTpt, resTpt)),
                    List(select: Term)
                  ),
                  List(monad, _)
                ),
                "returns"
              ),
              List(outTpt)
            ),
            List(apply: Term)
          ) =>
        Some((rest, select, apply, monad, effTpt, fixedErrorTpt(errorTpt), resTpt, argsTpt))
      case _ =>
        None
