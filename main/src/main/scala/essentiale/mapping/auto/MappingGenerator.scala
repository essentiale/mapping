package essentiale.mapping.auto

import essentiale.mapping.{GeneratedMapping, GeneratedRecursiveMapping, Mapping}

import scala.collection.generic.IsIterableOnce
import scala.collection.{mutable, BuildFrom}
import scala.reflect.macros.blackbox

/**
 * @author Konstantin Volchenko
 */
class MappingGenerator(val c: blackbox.Context) {

  import c.universe._

  private abstract sealed class MappingGenerationError

  private case class FieldAbsentError(fromTpe: c.Type, toTpe: c.Type, fieldName: String, ruleName: String) extends MappingGenerationError {

    override def toString: String =
      s"Mapping from $fromTpe to $toTpe could not be generated. Field $fieldName has no corresponding field in source class and rule for _.$ruleName not found."

  }

  private case class FieldNonConvertibleError(
    fromTpe: c.Type,
    toTpe: c.Type,
    fieldName: String,
    fieldFromTpe: c.Type,
    fieldToTpe: c.Type,
    cause: Option[MappingGenerationError] = None
  ) extends MappingGenerationError {

    override def toString: String = {
      val causeStr = cause.map(error => s" due to the following reason:\n${error.toString}").getOrElse(".")
      s"Mapping from $fromTpe to $toTpe could not be generated. Field $fieldName could not be converted from $fieldFromTpe to $fieldToTpe$causeStr"
    }

  }

  private case class TypeNonConvertibleError(fromTpe: c.Type, toTpe: c.Type) extends MappingGenerationError {

    override def toString: String = {
      s"Could not find mapping from $fromTpe to $toTpe."
    }

  }

  private case class TargetNotCaseClassError(tpe: c.Type) extends MappingGenerationError {

    override def toString: String =
      s"Mapping to type $tpe could not be generated. Only case classes with visible primary constructor are supported."

  }

  def buildMapping[S: c.WeakTypeTag, T: c.WeakTypeTag]: c.Tree = {
    val mapping = buildMappingInternal[S, T](Map.empty, q"${TermName("$mapping_src")}", recursive = false)
    val result  = q"""
        new ${typeOf[Mapping[_, _]].typeSymbol}[${weakTypeOf[S]}, ${weakTypeOf[T]}] {
            protected def useAllowEmptyOptionsMappingOption(implicit opt: ${typeOf[AllowEmptyOptionalMappingOption]}): Boolean = true
            protected def useAllowEmptyDefaultsMappingOption(implicit opt: ${typeOf[AllowEmptyDefaultsMappingOption]}): Boolean = true
            protected def useAllowImplicitConversionsMappingOption(implicit opt: ${typeOf[AllowImplicitConversionsMappingOption]}): Boolean = true
            protected def useDisableImplicitMappingsMappingOption(implicit opt: ${typeOf[DisableImplicitMappingsMappingOption]}): Boolean = true
            override def map($$mapping_src: ${weakTypeOf[S]}): ${weakTypeOf[T]} = $mapping
        }
       """
    result
  }

  def buildMappingRecursive[S: c.WeakTypeTag, T: c.WeakTypeTag]: c.Tree = {
    val mapping = buildMappingInternal[S, T](Map.empty, q"${TermName("$mapping_src")}", recursive = true)
    val result  = q"""
        new ${typeOf[Mapping[_, _]].typeSymbol}[${weakTypeOf[S]}, ${weakTypeOf[T]}] {
            protected def useAllowEmptyOptionsMappingOption(implicit opt: ${typeOf[AllowEmptyOptionalMappingOption]}): Boolean = true
            protected def useAllowEmptyDefaultsMappingOption(implicit opt: ${typeOf[AllowEmptyDefaultsMappingOption]}): Boolean = true
            protected def useAllowImplicitConversionsMappingOption(implicit opt: ${typeOf[AllowImplicitConversionsMappingOption]}): Boolean = true
            protected def useDisableImplicitMappingsMappingOption(implicit opt: ${typeOf[DisableImplicitMappingsMappingOption]}): Boolean = true
            override def map($$mapping_src: ${weakTypeOf[S]}): ${weakTypeOf[T]} = $mapping
        }
       """
    result
  }

  def mapInline[S: c.WeakTypeTag, T: c.WeakTypeTag]: c.Tree = {
    val mapping = buildMappingInternal[S, T](Map.empty, q"${TermName("$mapping_src")}", recursive = false)
    val result  = q"""
        new ${typeOf[GeneratedMapping[_, _]].typeSymbol}[${weakTypeOf[S]}, ${weakTypeOf[T]}] {
            protected def useAllowEmptyOptionsMappingOption(implicit opt: ${typeOf[AllowEmptyOptionalMappingOption]}): Boolean = true
            protected def useAllowEmptyDefaultsMappingOption(implicit opt: ${typeOf[AllowEmptyDefaultsMappingOption]}): Boolean = true
            protected def useAllowImplicitConversionsMappingOption(implicit opt: ${typeOf[AllowImplicitConversionsMappingOption]}): Boolean = true
            protected def useDisableImplicitMappingsMappingOption(implicit opt: ${typeOf[DisableImplicitMappingsMappingOption]}): Boolean = true
            override def map($$mapping_src: ${weakTypeOf[S]}): ${weakTypeOf[T]} = $mapping
        }
       """
    result
  }

  def mapInlineRecursive[S: c.WeakTypeTag, T: c.WeakTypeTag]: c.Tree = {
    val mapping = buildMappingInternal[S, T](Map.empty, q"${TermName("$mapping_src")}", recursive = true)
    val result  = q"""
        new ${typeOf[GeneratedRecursiveMapping[_, _]].typeSymbol}[${weakTypeOf[S]}, ${weakTypeOf[T]}] {
            protected def useAllowEmptyOptionsMappingOption(implicit opt: ${typeOf[AllowEmptyOptionalMappingOption]}): Unit = {}
            protected def useAllowEmptyDefaultsMappingOption(implicit opt: ${typeOf[AllowEmptyDefaultsMappingOption]}): Unit = {}
            protected def useAllowImplicitConversionsMappingOption(implicit opt: ${typeOf[AllowImplicitConversionsMappingOption]}): Unit = {}
            protected def useDisableImplicitMappingsMappingOption(implicit opt: ${typeOf[DisableImplicitMappingsMappingOption]}): Unit = {}
            override def map($$mapping_src: ${weakTypeOf[S]}): ${weakTypeOf[T]} = $mapping
        }
       """
    result
  }

  def buildMappingWithRules[S: c.WeakTypeTag, T: c.WeakTypeTag](rules: c.Expr[MappingRule[S, T]]*): c.Tree = {
    val mappings: mutable.Map[String, c.Tree] = mutable.Map()
    for (rule <- rules) {
      rule.tree match {
        case q"$_.this.mapping[$_]($setter)($getter)" =>
          setter match {
            case Function(_, Select(Ident(_), fieldName)) =>
              val name = fieldName.decodedName.toString
              if (mappings.isDefinedAt(name)) {
                c.abort(rule.tree.pos, s"Duplicate mapping rule for field: $name")
              } else {
                mappings.put(name, getter.asInstanceOf[c.Tree])
              }
            case _                                        =>
              c.abort(rule.tree.pos, "For building mapping rule you should specify target field as _.someField")
          }
        case _                                        =>
          c.abort(c.enclosingPosition, s"Unknown mapping rule: ${rule.tree}")
      }
    }
    val mapping = buildMappingInternal[S, T](mappings.toMap, q"${TermName("$mapping_src")}", recursive = false)
    val result = q"($$mapping_src => $mapping)"
    result
  }

  def buildMappingRecursiveWithRules[S: c.WeakTypeTag, T: c.WeakTypeTag](rules: c.Expr[MappingRule[S, T]]*): c.Tree = {
    val mappings: mutable.Map[String, c.Tree] = mutable.Map()
    for (rule <- rules) {
      rule.tree match {
        case q"$_.this.mapping[$_]($setter)($getter)" =>
          setter match {
            case Function(_, tree) =>
              val name = extractSelector(tree, rule.tree.pos).mkString(".")
              if (mappings.isDefinedAt(name)) {
                c.abort(rule.tree.pos, s"Duplicate mapping rule for field: $name")
              } else {
                mappings.put(name, getter.asInstanceOf[c.Tree])
              }
            case _                 =>
              c.abort(rule.tree.pos, "For building mapping rule you should specify target field as _.someField")
          }
        case _                                        =>
          c.abort(c.enclosingPosition, s"Unknown mapping rule: ${rule.tree}")
      }
    }
    val mapping = buildMappingInternal[S, T](mappings.toMap, q"${TermName("$mapping_src")}", recursive = true)
    val result = q"($$mapping_src => $mapping)"
    result
  }

  private def buildMappingInternal[S: c.WeakTypeTag, T: c.WeakTypeTag](
    mappings: Map[String, c.Tree],
    sourceVariableName: c.Tree,
    recursive: Boolean
  ): c.Tree = {
    val unusedRules = mutable.Set.from(mappings.keySet)
    val options     = readMappingOptions

    buildClassMapping(
      weakTypeOf[S],
      weakTypeOf[T],
      mappings,
      sourceVariableName,
      options,
      recursive,
      Seq.empty,
      unusedRules
    ) match {
      case Left(error)    =>
        c.abort(c.enclosingPosition, error.toString)
      case Right(mapping) =>
        for (rule <- unusedRules) {
          c.warning(mappings(rule).pos, s"Mapping rule for $rule field is not used.")
        }
        val result = q"""
        ${buildMacroImplicitUsage(options)}
        $mapping
       """
        result
    }
  }

  private def buildClassMapping(
    fromTpe: c.Type,
    toTpe: c.Type,
    mappings: Map[String, c.Tree],
    sourceVariableName: c.Tree,
    options: AutoMappingOptions,
    recursive: Boolean,
    targetPrefix: Seq[String],
    unusedRules: mutable.Set[String]
  ): Either[MappingGenerationError, c.Tree] = {
    getCaseClassConstructor(toTpe) match {
      case Some(targetConstructor) =>
        val targetConstructorSite = targetConstructor.typeSignatureIn(toTpe)
        targetConstructorSite.paramLists.head
          .map(x =>
            makeArgument(
              x,
              fromTpe,
              toTpe,
              x.typeSignatureIn(targetConstructorSite),
              mappings,
              options,
              fromTpe,
              sourceVariableName,
              recursive,
              targetPrefix,
              unusedRules
            )
          )
          .partitionMap(identity) match {
          case (Nil, args)          => Right(q"new $toTpe(..${args.flatten})")
          case (firstError :: _, _) => Left(firstError)
        }
      case None                    =>
        Left(TargetNotCaseClassError(toTpe))
    }
  }

  private def makeArgument(
    arg: c.Symbol,
    sourceClassTpe: c.Type,
    targetClassTpe: c.Type,
    toTpe: c.Type,
    mappings: Map[String, c.Tree],
    options: AutoMappingOptions,
    sourceType: c.Type,
    sourceVariableName: c.Tree,
    recursive: Boolean,
    targetPrefix: Seq[String],
    unusedRules: mutable.Set[String]
  ): Either[MappingGenerationError, Option[c.Tree]] = {
    val fieldName = (targetPrefix :+ arg.name.decodedName.toString).mkString(".")
    mappings.get(fieldName) match {
      case Some(value) =>
        unusedRules.remove(fieldName)
        Right(Some(q"${TermName(arg.name.decodedName.toString)} = $value($$mapping_src)"))
      case None        =>
        sourceType.members
          .filter(field => field.isMethod && field.asMethod.paramLists.isEmpty && field.asMethod.isPublic)
          .find(_.name.decodedName.toString == arg.name.decodedName.toString) match {
          case Some(value) =>
            val fromTpe = value.asMethod.returnType.asSeenFrom(sourceType, sourceType.typeSymbol)
            mapTypeRecursive(
              fromTpe,
              toTpe,
              mappings,
              options,
              q"$sourceVariableName.$value",
              targetPrefix :+ arg.name.decodedName.toString,
              unusedRules,
              fieldName,
              recursive
            )
              .map { tree =>
                Some(q"${TermName(arg.name.decodedName.toString)} = $tree")
              }
              .left
              .map { cause =>
                println(cause)
                FieldNonConvertibleError(sourceClassTpe, targetClassTpe, fieldName, fromTpe, toTpe, Some(cause))
              }
          case None        =>
            if (arg.asTerm.isParamWithDefault && options.allowEmptyDefaults) {
              Right(None)
            } else if (arg.typeSignature.typeSymbol == typeOf[Option[_]].typeSymbol && options.allowEmptyOptional) {
              Right(Some(q"${TermName(arg.name.decodedName.toString)} = None"))
            } else {
              Left(FieldAbsentError(sourceClassTpe, targetClassTpe, arg.name.decodedName.toString, fieldName))
            }
        }
    }
  }

  private def mapTypeRecursive(
    fromTpe: c.Type,
    toTpe: c.Type,
    mappings: Map[String, c.Tree],
    options: AutoMappingOptions,
    sourceVariableName: c.Tree,
    targetPrefix: Seq[String],
    unusedRules: mutable.Set[String],
    fieldName: String,
    recursive: Boolean
  ): Either[MappingGenerationError, c.Tree] = {
    tryEqualTypeMapping(fromTpe, toTpe, sourceVariableName)
      .orElse(tryImplicitTypeMapping(fromTpe, toTpe, sourceVariableName, options))
      .orElse(tryAssignableTypeMapping(fromTpe, toTpe, sourceVariableName, options))
      .orElse(
        tryCaseClassTypeMapping(fromTpe, toTpe, sourceVariableName, mappings, targetPrefix, unusedRules, options, recursive)
      )
      .orElse(tryOptionTypeMapping(fromTpe, toTpe, sourceVariableName, targetPrefix, options, fieldName, recursive))
      .orElse(tryEitherTypeMapping(fromTpe, toTpe, sourceVariableName, targetPrefix, options, fieldName, recursive))
      .orElse(tryIterableTypeMapping(fromTpe, toTpe, sourceVariableName, targetPrefix, options, fieldName, recursive))
      .getOrElse(Left(TypeNonConvertibleError(fromTpe, toTpe)))
  }

  private def tryEqualTypeMapping(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree
  ): Option[Either[MappingGenerationError, c.Tree]] = {
    if (fromTpe.=:=(toTpe)) {
      Some(Right(q"$sourceVariableName"))
    } else {
      None
    }
  }

  private def tryImplicitTypeMapping(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    options: AutoMappingOptions
  ): Option[Either[MappingGenerationError, c.Tree]] = {
    val implicitMapping = if (options.allowImplicitMappings) {
      val mappingType = appliedType(typeOf[Mapping[_, _]], fromTpe, toTpe)
      c.inferImplicitValue(mappingType)
    } else {
      EmptyTree
    }
    if (implicitMapping.nonEmpty) {
      Some(Right(q"$implicitMapping.map($sourceVariableName)"))
    } else {
      None
    }
  }

  private def tryAssignableTypeMapping(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    options: AutoMappingOptions
  ): Option[Either[MappingGenerationError, c.Tree]] = {
    if (isAssignable(fromTpe, toTpe, options)) {
      Some(Right(q"$sourceVariableName"))
    } else {
      None
    }
  }

  private def tryCaseClassTypeMapping(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    mappings: Map[String, c.Tree],
    targetPrefix: Seq[String],
    unusedRules: mutable.Set[String],
    options: AutoMappingOptions,
    recursive: Boolean
  ): Option[Either[MappingGenerationError, c.Tree]] = {
    if (recursive && fromTpe.typeSymbol.isClass && toTpe.typeSymbol.isClass && toTpe.typeSymbol.asClass.isCaseClass) {
      Some(
        buildClassMapping(
          fromTpe,
          toTpe,
          mappings,
          q"$sourceVariableName",
          options,
          recursive = true,
          targetPrefix,
          unusedRules
        )
      )
    } else {
      None
    }
  }

  private def tryOptionTypeMapping(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    targetPrefix: Seq[String],
    options: AutoMappingOptions,
    fieldName: String,
    recursive: Boolean
  ): Option[Either[MappingGenerationError, c.Tree]] = {
    if (recursive && fromTpe.typeSymbol == typeOf[Option[_]].typeSymbol && toTpe.typeSymbol == typeOf[Option[_]].typeSymbol) {
      val freeName = c.freshName
      Some(
        mapTypeRecursive(
          fromTpe.typeArgs.head,
          toTpe.typeArgs.head,
          Map.empty,
          options,
          q"${TermName(freeName)}",
          targetPrefix,
          mutable.Set.empty,
          fieldName,
          recursive
        ).map { subTree =>
          q"$sourceVariableName.map(${makeLambdaArgument(freeName)} => $subTree)"
        }
      )
    } else {
      None
    }
  }

  private def tryEitherTypeMapping(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    targetPrefix: Seq[String],
    options: AutoMappingOptions,
    fieldName: String,
    recursive: Boolean
  ): Option[Either[MappingGenerationError, c.Tree]] = {
    if (
      recursive &&
      fromTpe.baseClasses.contains(typeOf[Either[_, _]].typeSymbol) && toTpe.baseClasses.contains(typeOf[Either[_, _]].typeSymbol)
    ) {
      val leftEitherType       = typeOf[Either[_, _]].typeSymbol.asClass.typeParams.head.asType.toType
      val rightEitherType      = typeOf[Either[_, _]].typeSymbol.asClass.typeParams.tail.head.asType.toType
      val fromLeftElementType  = leftEitherType.asSeenFrom(fromTpe, typeOf[Either[_, _]].typeSymbol)
      val fromRightElementType = rightEitherType.asSeenFrom(fromTpe, typeOf[Either[_, _]].typeSymbol)
      val toLeftElementType    = leftEitherType.asSeenFrom(toTpe, typeOf[Either[_, _]].typeSymbol)
      val toRightElementType   = rightEitherType.asSeenFrom(toTpe, typeOf[Either[_, _]].typeSymbol)
      val tpt                  = tq""
      val leftParamName        = c.freshName
      val leftParam            = q"val ${TermName(leftParamName)}: $tpt"
      val rightParamName       = c.freshName
      val rightParam           = q"val ${TermName(rightParamName)}: $tpt"
      Some(for {
        leftSubTree  <- mapTypeRecursive(
                          fromLeftElementType,
                          toLeftElementType,
                          Map.empty,
                          options,
                          q"${TermName(leftParamName)}",
                          targetPrefix,
                          mutable.Set.empty,
                          fieldName,
                          recursive
                        )
        rightSubTree <- mapTypeRecursive(
                          fromRightElementType,
                          toRightElementType,
                          Map.empty,
                          options,
                          q"${TermName(rightParamName)}",
                          targetPrefix,
                          mutable.Set.empty,
                          fieldName,
                          recursive
                        )
      } yield q"""$sourceVariableName.map($rightParam => $rightSubTree).left.map($leftParam => $leftSubTree)""")
    } else {
      None
    }
  }

  private def tryIterableTypeMapping(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    targetPrefix: Seq[String],
    options: AutoMappingOptions,
    fieldName: String,
    recursive: Boolean
  ): Option[Either[MappingGenerationError, c.Tree]] = {
    if (recursive) {
      val fromIsIterable = findIterableOnce(fromTpe)
      val toIsIterable   = findIterableOnce(toTpe)
      (fromIsIterable, toIsIterable) match {
        case (Some((fromElemTpe, fromIterableOnceImplicit)), Some((toElemTpe, toIterableOnceImplicit))) =>
          val buildFromImplicit = c.inferImplicitValue(appliedType(typeOf[BuildFrom[_, _, _]], fromTpe, toElemTpe, toTpe))
          if (fromIterableOnceImplicit.nonEmpty && toIterableOnceImplicit.nonEmpty && buildFromImplicit.nonEmpty) {
            val freeName      = c.freshName
            val freeParamName = c.freshName
            val tpt           = tq""
            val param         = q"val ${TermName(freeParamName)}: $tpt"
            Some(
              mapTypeRecursive(
                fromElemTpe,
                toElemTpe,
                Map.empty,
                options,
                q"${TermName(freeParamName)}",
                targetPrefix,
                mutable.Set.empty,
                fieldName,
                recursive
              )
                .map { subTree =>
                  q""" {
             val ${TermName(freeName)} = $sourceVariableName
             $buildFromImplicit.fromSpecific(${TermName(freeName)})($fromIterableOnceImplicit.apply(${TermName(freeName)}).iterator.map($param => $subTree))
           }
        """
                }
            )
          } else {
            None
          }
        case _                                                                                          =>
          None
      }
    } else {
      None
    }
  }

  private def findIterableOnce(tpe: c.Type): Option[(c.Type, c.Tree)] = {
    val elementType = if (tpe.baseClasses.contains(typeOf[IterableOnce[_]].typeSymbol)) {
      val iterableElementType = typeOf[IterableOnce[_]].typeSymbol.asClass.typeParams.head.asType.toType
      Some(iterableElementType.asSeenFrom(tpe, typeOf[IterableOnce[_]].typeSymbol))
    } else if (tpe.baseClasses.contains(typeOf[Array[_]].typeSymbol)) {
      val arrayElementType = typeOf[Array[_]].typeSymbol.asClass.typeParams.head.asType.toType
      Some(arrayElementType.asSeenFrom(tpe, typeOf[Array[_]].typeSymbol))
    } else {
      None
    }
    elementType.flatMap { elTpe =>
      val iterableOnceImplicit = c.inferImplicitValue(appliedType(typeOf[IsIterableOnce[_]], tpe))
      if (iterableOnceImplicit.isEmpty)
        None
      else
        Some((elTpe, iterableOnceImplicit))
    }
  }

  private def makeLambdaArgument(freeName: String): c.Tree = {
    val tpt = tq""
    q"val ${TermName(freeName)}: $tpt"
  }

  private def isAssignable(fromTpe: c.Type, toTpe: c.Type, options: AutoMappingOptions): Boolean = {
    if (fromTpe <:< toTpe) {
      true
    } else if (options.allowImplicitConversions && c.inferImplicitValue(appliedType(typeOf[_ => _], fromTpe, toTpe)).nonEmpty) {
      true
    } else if (options.allowImplicitConversions && c.inferImplicitView(EmptyTree, fromTpe, toTpe).nonEmpty) {
      true
    } else {
      false
    }
  }

  private def readMappingOptions: AutoMappingOptions = {
    val allowEmptyOptional       = c.inferImplicitValue(c.universe.typeOf[AllowEmptyOptionalMappingOption]).nonEmpty
    val allowEmptyDefaults       = c.inferImplicitValue(c.universe.typeOf[AllowEmptyDefaultsMappingOption]).nonEmpty
    val allowImplicitConversions = c.inferImplicitValue(c.universe.typeOf[AllowImplicitConversionsMappingOption]).nonEmpty
    val allowImplicitMappings    = c.inferImplicitValue(c.universe.typeOf[DisableImplicitMappingsMappingOption]).isEmpty
    AutoMappingOptions(allowEmptyOptional, allowEmptyDefaults, allowImplicitConversions, allowImplicitMappings)
  }

  private def buildMacroImplicitUsage(options: AutoMappingOptions): c.Tree = {
    val opt1 =
      if (options.allowEmptyOptional)
        q"scala.Predef.assert(useAllowEmptyOptionsMappingOption)"
      else
        EmptyTree
    val opt2 =
      if (options.allowEmptyDefaults)
        q"scala.Predef.assert(useAllowEmptyDefaultsMappingOption)"
      else
        EmptyTree
    val opt3 =
      if (options.allowImplicitConversions)
        q"scala.Predef.assert(useAllowImplicitConversionsMappingOption)"
      else
        EmptyTree
    val opt4 =
      if (!options.allowImplicitMappings)
        q"scala.Predef.assert(useDisableImplicitMappingsMappingOption)"
      else
        EmptyTree
    if (opt1.isEmpty && opt2.isEmpty && opt3.isEmpty && opt4.isEmpty)
      EmptyTree
    else
      q"""
        $opt1
        $opt2
        $opt3
        $opt4
       """
  }

  private def extractSelector(tree: c.Tree, pos: c.Position): Seq[String] = {
    tree match {
      case Ident(_)                   => Seq.empty
      case Select(subtree, fieldName) =>
        val name = fieldName.decodedName.toString
        extractSelector(subtree, pos) :+ name
      case _                          =>
        c.abort(pos, s"For building mapping rule you should specify target field as _.someField.")
    }
  }

  private def getCaseClassConstructor(tpe: c.Type): Option[c.universe.MethodSymbol] = {
    if (!tpe.typeSymbol.isClass)
      None
    else if (!tpe.typeSymbol.asClass.isCaseClass)
      None
    else {
      tpe.decls.collectFirst {
        case method: MethodSymbol if method.isPrimaryConstructor => method
      }
    }
  }

}

private case class AutoMappingOptions(
  allowEmptyOptional: Boolean,
  allowEmptyDefaults: Boolean,
  allowImplicitConversions: Boolean,
  allowImplicitMappings: Boolean
)
