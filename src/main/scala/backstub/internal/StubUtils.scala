package backstub.internal

import scala.annotation.tailrec
import scala.quoted.*

private[backstub] trait StubUtils(using
    val quotes: Quotes
):
  import quotes.reflect.*

  def tupled(args: List[Term]) =
    args.foldRight[Term]('{ EmptyTuple }.asTerm) { (el, acc) =>
      Select
        .unique(acc, "*:")
        .appliedToTypes(List(el.tpe, acc.tpe))
        .appliedToArgs(List(el))
    }

  def methodsOf(tpe: TypeRepr): List[Method] =
    (tpe.typeSymbol.methodMembers.toSet -- TypeRepr.of[Object].typeSymbol.methodMembers).toList
      .filter(sym =>
        !sym.flags.is(Flags.Private) &&
          !sym.flags.is(Flags.Final) &&
          !sym.flags.is(Flags.Mutable) &&
          sym.privateWithin.isEmpty &&
          !sym.name.contains("$default$")
      )
      .zipWithIndex.map(Method(_, _))

  case class Method(symbol: Symbol, idx: Int):
    val callsValName = s"calls$$${symbol.name}$$$idx"
    val paramNames: List[String] = symbol.paramSymss.flatten.filterNot(_.isAbstractType).map(_.name)

  extension (methods: List[Method])
    def searchMethod(select: Term, argsTpe: Option[TypeRepr], resTpe: TypeRepr): Method =
      val (name, appliedTypes) = searchMethodNameAndAppliedTypes(select)

      @tailrec
      def resolveTuples(tpe: TypeRepr, acc: List[TypeRepr] = Nil): List[TypeRepr] =
        tpe.asType match
          case '[h *: t] =>
            resolveTuples(TypeRepr.of[t], acc :+ TypeRepr.of[h])
          case '[EmptyTuple] =>
            acc
          case _ =>
            acc :+ tpe

      val argsTpes = argsTpe match
        case Some(AppliedType(_, types)) => types.flatMap(resolveTuples(_))
        case None                        => Nil
        case Some(tpe) =>
          List(tpe)

      methods.find { method =>
        val (methodArgsTpes, methodResTpe) = method.symbol.info match
          case MethodType(_, types, resTpe) =>
            (types, resTpe)
          case ByNameType(tpe) =>
            (Nil, tpe)
          case tpe =>
            (Nil, TypeRepr.of[Nothing])

        name == method.symbol.name &&
        resTpe <:< methodResTpe &&
        argsTpes.zipAll(methodArgsTpes, TypeRepr.of[Any], TypeRepr.of[Nothing]).forall(_ <:< _)
      }.getOrElse(report.errorAndAbort(s"Method signature not yet supported: ${select.show}"))

  private def searchMethodNameAndAppliedTypes(select: Term) =
    @tailrec
    def transcribeTree(term: Term, types: List[TypeTree] = Nil): (String, List[TypeTree]) =
      term match
        case Select(_, name)                             => (name, types)
        case Inlined(_, _, term)                         => transcribeTree(term)
        case Block(List(DefDef(_, _, _, Some(term))), _) => transcribeTree(term)
        case Typed(term, _)                              => transcribeTree(term)
        case Lambda(_, term)                             => transcribeTree(term)
        case Apply(term, _)                              => transcribeTree(term)
        case TypeApply(term, types)                      => transcribeTree(term, types)
        case _ =>
          report.errorAndAbort(
            s"Unrecognised tree structure: ${term.show(using
              Printer.TreeStructure)}"
          )

    transcribeTree(select)
