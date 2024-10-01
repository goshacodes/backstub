package backstub.internal

import scala.annotation.tailrec
import scala.quoted.*

private[backstub] trait StubUtils(using
    val quotes: Quotes
):
  import quotes.reflect.*
  
  val clearStubsMethodName = "stubs$macro$clear"

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
    val (argTypes, resTpe) = resolveMethodType(symbol.info)

    def resolveParamRefs(resTpe: TypeRepr, methodArgs: List[List[Tree]]): TypeRepr =
      val res = symbol.info match
        case baseBindings: PolyType =>
          def loop(typeRepr: TypeRepr): TypeRepr =
            typeRepr match
              case pr@ParamRef(bindings, idx) if bindings == baseBindings =>
                methodArgs.head(idx).asInstanceOf[TypeTree].tpe

              case AndType(left, right) =>
                AndType(loop(left), loop(right))

              case OrType(left, right) =>
                OrType(loop(left), loop(right))

              case AppliedType(tycon, args) =>
                AppliedType(loop(tycon), args.map(arg => loop(arg)))

              case ff@TypeRef(ref@ParamRef(bindings, idx), name) =>
                def getIndex(bindings: TypeRepr): Int =
                  @tailrec
                  def loop(bindings: TypeRepr, idx: Int): Int =
                    bindings match
                      case MethodType(_, _, method: MethodType) => loop(method, idx + 1)
                      case _ => idx

                  loop(bindings, 1)

                val maxIndex = methodArgs.length
                val parameterListIdx = maxIndex - getIndex(bindings)

                TypeSelect(methodArgs(parameterListIdx)(idx).asInstanceOf[Term], name).tpe

              case other => other

          loop(resTpe)
        case _ =>
          resTpe

      res match
        case ParamRef(_: NoPrefix, _) =>
          TypeRepr.of[Nothing]
        case _ =>
          res


    private def resolveMethodType(tpe: TypeRepr): (List[TypeRepr], TypeRepr) =
      tpe match
        case PolyType(_, _, resType: MethodType) =>
          resolveMethodType(resType)

        case MethodType(_, types, resTpe: MethodType) =>
          val (otherTypes, res) = resolveMethodType(resTpe)
          (types ++ otherTypes, res)

        case MethodType(_, types, resTpe) =>
          (types, resTpe)

        case ByNameType(tpe) =>
          (Nil, tpe)

        case tpe =>
          (Nil, TypeRepr.of[Nothing])


  extension (methods: List[Method])
    def searchMethod(select: Term, argsTpe: Option[TypeRepr], resTpe: TypeRepr): Method =
      val (name, appliedTypes) = searchMethodNameAndAppliedTypes(select)
      
      def resolveResType(name: String, tpe: TypeRepr, args: List[TypeRepr]): (List[TypeRepr], TypeRepr) =
        val isFunction = tpe.typeSymbol.name.contains("Function") && tpe.typeSymbol.owner.name == "scala"
        tpe match
          case AppliedType(tycon, types) if isFunction =>
            resolveResType(name, types.last, args ++ types.init)
          case other =>
            (args, other)

      @tailrec
      def resolveTuples(tpe: TypeRepr, acc: List[TypeRepr] = Nil): List[TypeRepr] =
        tpe.asType match
          case '[h *: t] =>
            resolveTuples(TypeRepr.of[t], acc :+ TypeRepr.of[h])
          case '[EmptyTuple] =>
            acc
          case _ =>
            acc :+ tpe

      def mapParamRefWithWildcard(tpe: TypeRepr): TypeRepr =
        tpe match
          case ParamRef(PolyType(_, bounds, _), idx) if appliedTypes.nonEmpty =>
            appliedTypes(idx).tpe
          case AppliedType(tycon, args) if appliedTypes.nonEmpty =>
            tycon.appliedTo(args.map(mapParamRefWithWildcard(_)))
          case _ =>
            tpe

      def resolveParamRefs(tpe: TypeRepr): TypeRepr =
        mapParamRefWithWildcard(tpe)


      val argsTpes = argsTpe match
        case Some(AppliedType(_, types)) => types.flatMap(resolveTuples(_))
        case None                        => Nil
        case Some(tpe) =>
          List(tpe)

      methods.find { method =>
        val (resTypeArgsTpes, finalResTpe) = resolveResType(name, resTpe, Nil)
        val finalArgTpes = argsTpes ++ resTypeArgsTpes

        name == method.symbol.name &&
        finalResTpe <:< resolveParamRefs(method.resTpe) &&
        finalArgTpes.zipAll(method.argTypes.map(resolveParamRefs(_)), TypeRepr.of[Any], TypeRepr.of[Nothing]).forall(_ <:< _)
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
