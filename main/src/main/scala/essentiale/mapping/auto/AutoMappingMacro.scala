package essentiale.mapping.auto

import essentiale.mapping.{GeneratedMapping, GeneratedRecursiveMapping, Mapping}

import scala.collection.generic.IsIterableOnce
import scala.collection.{mutable, BuildFrom}
import scala.reflect.macros.blackbox

/**
 * @author Konstantin Volchenko
 */
object AutoMappingMacro {

  private case class AutoMappingOptions(
    allowEmptyOptional: Boolean,
    allowEmptyDefaults: Boolean,
    allowImplicitConversions: Boolean,
    allowImplicitMappings: Boolean
  )

  def buildMapping[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val mapping = buildMappingInternal[S, T](c)(Map.empty, q"${TermName("$mapping_src")}", recursive = false)
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

  def buildMappingRecursive[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val mapping = buildMappingInternal[S, T](c)(Map.empty, q"${TermName("$mapping_src")}", recursive = true)
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

  def mapInline[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val mapping = buildMappingInternal[S, T](c)(Map.empty, q"${TermName("$mapping_src")}", recursive = false)
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

  def mapInlineRecursive[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    val mapping = buildMappingInternal[S, T](c)(Map.empty, q"${TermName("$mapping_src")}", recursive = true)
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

  def buildMappingWithRules[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context)(rules: c.Expr[MappingRule[S, T]]*): c.Tree = {
    import c.universe._
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
    val mapping = buildMappingInternal[S, T](c)(mappings.toMap, q"${TermName("$mapping_src")}", recursive = false)
    val result = q"($$mapping_src => $mapping)"
    result
  }

  def buildMappingRecusiveWithRules[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context)(rules: c.Expr[MappingRule[S, T]]*): c.Tree = {
    import c.universe._
    val mappings: mutable.Map[String, c.Tree] = mutable.Map()
    for (rule <- rules) {
      rule.tree match {
        case q"$_.this.mapping[$_]($setter)($getter)" =>
          setter match {
            case Function(_, tree) =>
              val name = extractSelector(c)(tree, rule.tree.pos).mkString(".")
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
    val mapping = buildMappingInternal[S, T](c)(mappings.toMap, q"${TermName("$mapping_src")}", recursive = true)
    val result = q"($$mapping_src => $mapping)"
    result
  }

  private def buildMappingInternal[S: c.WeakTypeTag, T: c.WeakTypeTag](
    c: blackbox.Context
  )(mappings: Map[String, c.Tree], sourceVariableName: c.Tree, recursive: Boolean): c.Tree = {
    import c.universe._
    val unusedRules = mutable.Set.from(mappings.keySet)
    val options     = readMappingOptions(c)
    val mapping     =
      buildClassMapping(c)(
        weakTypeOf[S],
        weakTypeOf[T],
        mappings,
        sourceVariableName,
        options,
        recursive,
        topLevel = true,
        Seq.empty,
        unusedRules
      )
    for (rule <- unusedRules) {
      c.warning(mappings(rule).pos, s"Mapping rule for $rule field is not used.")
    }
    val result = q"""
        ${buildMacroImplicitUsage(c)(options)}
        $mapping
       """
    result
  }

  private def buildClassMapping(
    c: blackbox.Context
  )(
    fromTpe: c.Type,
    toTpe: c.Type,
    mappings: Map[String, c.Tree],
    sourceVariableName: c.Tree,
    options: AutoMappingOptions,
    recursive: Boolean,
    topLevel: Boolean,
    targetPrefix: Seq[String],
    unusedRules: mutable.Set[String]
  ): c.Tree = {
    import c.universe._
    val targetConstructor     = getCaseClassConstructor(c)(toTpe)
    val targetConstructorSite = targetConstructor.typeSignatureIn(toTpe)
    val args                  = targetConstructorSite.paramLists.head.flatMap(x =>
      makeArgument(c)(
        x,
        x.typeSignatureIn(targetConstructorSite),
        mappings,
        options,
        fromTpe,
        sourceVariableName,
        recursive,
        topLevel,
        targetPrefix,
        unusedRules
      )
    )
    val result                = q"new $toTpe(..$args)"
    result
  }

  private def makeArgument(c: blackbox.Context)(
    arg: c.Symbol,
    toTpe: c.Type,
    mappings: Map[String, c.Tree],
    options: AutoMappingOptions,
    sourceType: c.Type,
    sourceVariableName: c.Tree,
    recursive: Boolean,
    topLevel: Boolean,
    targetPrefix: Seq[String],
    unusedRules: mutable.Set[String]
  ): Option[c.Tree] = {
    import c.universe._
    val fieldName = (targetPrefix :+ arg.name.decodedName.toString).mkString(".")
    mappings.get(fieldName) match {
      case Some(value) =>
        unusedRules.remove(fieldName)
        Some(q"${TermName(arg.name.decodedName.toString)} = $value($$mapping_src)")
      case None        =>
        sourceType.members
          .filter(field => field.isMethod && field.asMethod.paramLists.isEmpty && field.asMethod.isPublic)
          .find(_.name.decodedName.toString == arg.name.decodedName.toString) match {
          case Some(value) =>
            val fromTpe = value.asMethod.returnType.asSeenFrom(sourceType, sourceType.typeSymbol)
            val tree    = mapTypeRecursive(c)(
              fromTpe,
              toTpe,
              mappings,
              options,
              q"$sourceVariableName.$value",
              targetPrefix :+ arg.name.decodedName.toString,
              unusedRules,
              fieldName,
              recursive,
              topLevel
            ).getOrElse(
              c.abort(
                c.enclosingPosition,
                s"Could not convert value of field $fieldName from source type ${fromTpe} to target type ${toTpe}"
              )
            )
            Some(q"${TermName(arg.name.decodedName.toString)} = $tree")
          case None        =>
            if (arg.asTerm.isParamWithDefault && options.allowEmptyDefaults) {
              None
            } else if (arg.typeSignature.typeSymbol == typeOf[Option[_]].typeSymbol && options.allowEmptyOptional) {
              Some(q"${TermName(arg.name.decodedName.toString)} = None")
            } else {
              c.abort(c.enclosingPosition, s"Mapping for field $fieldName not found")
            }
        }
    }
  }

  private def mapTypeRecursive(c: blackbox.Context)(
    fromTpe: c.Type,
    toTpe: c.Type,
    mappings: Map[String, c.Tree],
    options: AutoMappingOptions,
    sourceVariableName: c.Tree,
    targetPrefix: Seq[String],
    unusedRules: mutable.Set[String],
    fieldName: String,
    recursive: Boolean,
    topLevel: Boolean
  ): Option[c.Tree] = {
    tryEqualTypeMapping(c)(fromTpe, toTpe, sourceVariableName)
      .orElse(tryImplicitTypeMapping(c)(fromTpe, toTpe, sourceVariableName, options))
      .orElse(tryAssignableTypeMapping(c)(fromTpe, toTpe, sourceVariableName, options))
      .orElse(
        tryCaseClassTypeMapping(c)(fromTpe, toTpe, sourceVariableName, mappings, targetPrefix, unusedRules, options, recursive, topLevel)
      )
      .orElse(tryOptionTypeMapping(c)(fromTpe, toTpe, sourceVariableName, targetPrefix, options, fieldName, recursive))
      .orElse(tryEitherTypeMapping(c)(fromTpe, toTpe, sourceVariableName, targetPrefix, options, fieldName, recursive))
      .orElse(tryIterableTypeMapping(c)(fromTpe, toTpe, sourceVariableName, targetPrefix, options, fieldName, recursive))
  }

  private def tryImplicitTypeMapping(c: blackbox.Context)(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    options: AutoMappingOptions
  ): Option[c.Tree] = {
    import c.universe._
    val implicitMapping = findImplicitMapping(c)(fromTpe, toTpe, options)
    if (implicitMapping.nonEmpty) {
      Some(q"implicitly[${appliedType(typeOf[Mapping[_, _]], fromTpe, toTpe)}].map($sourceVariableName)")
    } else {
      None
    }
  }

  private def tryEqualTypeMapping(c: blackbox.Context)(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree
  ): Option[c.Tree] = {
    import c.universe._
    if (fromTpe.=:=(toTpe)) {
      Some(q"$sourceVariableName")
    } else {
      None
    }
  }

  private def tryAssignableTypeMapping(c: blackbox.Context)(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    options: AutoMappingOptions
  ): Option[c.Tree] = {
    import c.universe._
    if (isAssignable(c)(fromTpe, toTpe, options)) {
      Some(q"$sourceVariableName")
    } else {
      None
    }
  }

  private def tryCaseClassTypeMapping(c: blackbox.Context)(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    mappings: Map[String, c.Tree],
    targetPrefix: Seq[String],
    unusedRules: mutable.Set[String],
    options: AutoMappingOptions,
    recursive: Boolean,
    topLevel: Boolean
  ): Option[c.Tree] = {
    import c.universe._
    if (recursive && fromTpe.typeSymbol.isClass && toTpe.typeSymbol.isClass && toTpe.typeSymbol.asClass.isCaseClass) {
      Some(
        buildClassMapping(c)(
          fromTpe,
          toTpe,
          mappings,
          q"$sourceVariableName",
          options,
          recursive = true,
          topLevel,
          targetPrefix,
          unusedRules
        )
      )
    } else {
      None
    }
  }

  private def tryOptionTypeMapping(c: blackbox.Context)(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    targetPrefix: Seq[String],
    options: AutoMappingOptions,
    fieldName: String,
    recursive: Boolean
  ): Option[c.Tree] = {
    import c.universe._
    if (recursive && fromTpe.typeSymbol == typeOf[Option[_]].typeSymbol && toTpe.typeSymbol == typeOf[Option[_]].typeSymbol) {
      val freeName = c.freshName
      c.enclosingPosition
      mapTypeRecursive(c)(
        fromTpe.typeArgs.head,
        toTpe.typeArgs.head,
        Map.empty,
        options,
        q"${TermName(freeName)}",
        targetPrefix,
        mutable.Set.empty,
        fieldName,
        recursive,
        topLevel = false
      ).map { subTree =>
        val tpt   = tq""
        val param = q"val ${TermName(freeName)}: $tpt"
        q"$sourceVariableName.map($param => $subTree)"
      }.orElse(
        Some(
          c.abort(c.enclosingPosition, s"Could not convert value of field $fieldName from source type ${fromTpe} to target type ${toTpe}")
        )
      )
    } else {
      None
    }
  }

  private def tryEitherTypeMapping(c: blackbox.Context)(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    targetPrefix: Seq[String],
    options: AutoMappingOptions,
    fieldName: String,
    recursive: Boolean
  ): Option[c.Tree] = {
    import c.universe._
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
      (for {
        leftSubTree  <- mapTypeRecursive(c)(
                          fromLeftElementType,
                          toLeftElementType,
                          Map.empty,
                          options,
                          q"${TermName(leftParamName)}",
                          targetPrefix,
                          mutable.Set.empty,
                          fieldName,
                          recursive,
                          topLevel = false
                        )
        rightSubTree <- mapTypeRecursive(c)(
                          fromRightElementType,
                          toRightElementType,
                          Map.empty,
                          options,
                          q"${TermName(rightParamName)}",
                          targetPrefix,
                          mutable.Set.empty,
                          fieldName,
                          recursive,
                          topLevel = false
                        )
      } yield q"""$sourceVariableName.map($rightParam => $rightSubTree).left.map($leftParam => $leftSubTree)""")
        .orElse(
          Some(
            c.abort(c.enclosingPosition, s"Could not convert value of field $fieldName from source type ${fromTpe} to target type ${toTpe}")
          )
        )
    } else {
      None
    }
  }

  private def tryIterableTypeMapping(c: blackbox.Context)(
    fromTpe: c.Type,
    toTpe: c.Type,
    sourceVariableName: c.Tree,
    targetPrefix: Seq[String],
    options: AutoMappingOptions,
    fieldName: String,
    recursive: Boolean
  ): Option[c.Tree] = {
    import c.universe._
    if (recursive) {
      val fromIsIterable = findIterableOnce(c)(fromTpe)
      val toIsIterable   = findIterableOnce(c)(toTpe)
      (fromIsIterable, toIsIterable) match {
        case (Some((fromElemTpe, fromIterableOnceImplicit)), Some((toElemTpe, toIterableOnceImplicit))) =>
          val buildFromImplicit = c.inferImplicitValue(appliedType(typeOf[BuildFrom[_, _, _]], fromTpe, toElemTpe, toTpe))
          if (fromIterableOnceImplicit.nonEmpty && toIterableOnceImplicit.nonEmpty && buildFromImplicit.nonEmpty) {
            val freeName      = c.freshName
            val freeParamName = c.freshName
            val tpt           = tq""
            val param         = q"val ${TermName(freeParamName)}: $tpt"
            mapTypeRecursive(c)(
              fromElemTpe,
              toElemTpe,
              Map.empty,
              options,
              q"${TermName(freeParamName)}",
              targetPrefix,
              mutable.Set.empty,
              fieldName,
              recursive,
              topLevel = false
            )
              .map { subTree =>
                q""" {
             val ${TermName(freeName)} = $sourceVariableName
             $buildFromImplicit.fromSpecific(${TermName(freeName)})($fromIterableOnceImplicit.apply(${TermName(freeName)}).iterator.map($param => $subTree))
           }
        """
              }
              .orElse(
                c.abort(
                  c.enclosingPosition,
                  s"Could not convert value of field $fieldName from source type ${fromTpe} to target type ${toTpe}"
                )
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

  private def findIterableOnce(c: blackbox.Context)(tpe: c.Type): Option[(c.Type, c.Tree)] = {
    import c.universe._
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

  private def findImplicitMapping(c: blackbox.Context)(fromTpe: c.Type, toTpe: c.Type, options: AutoMappingOptions): c.Tree = {
    import c.universe._
    if (options.allowImplicitMappings) {
      val mappingType = appliedType(typeOf[Mapping[_, _]], fromTpe, toTpe)
      c.inferImplicitValue(mappingType)
    } else {
      EmptyTree
    }
  }

  private def isAssignable(c: blackbox.Context)(fromTpe: c.Type, toTpe: c.Type, options: AutoMappingOptions): Boolean = {
    import c.universe._
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

  private def getCaseClassConstructor(c: blackbox.Context)(tpe: c.Type): c.universe.MethodSymbol = {
    import c.universe._
    if (!tpe.typeSymbol.isClass)
      c.abort(c.enclosingPosition, "Auto mapping could be build only for case classes.")
    if (!tpe.typeSymbol.asClass.isCaseClass)
      c.abort(c.enclosingPosition, "Auto mapping could be build only for case classes.")
    val ctor = tpe.decls
      .collectFirst {
        case m: MethodSymbol if m.isPrimaryConstructor => m
      }
      .getOrElse(c.abort(c.enclosingPosition, s"Case class constructor is absent for class $tpe."))
    ctor
  }

  private def readMappingOptions(c: blackbox.Context): AutoMappingOptions = {
    val allowEmptyOptional       = c.inferImplicitValue(c.universe.typeOf[AllowEmptyOptionalMappingOption]).nonEmpty
    val allowEmptyDefaults       = c.inferImplicitValue(c.universe.typeOf[AllowEmptyDefaultsMappingOption]).nonEmpty
    val allowImplicitConversions = c.inferImplicitValue(c.universe.typeOf[AllowImplicitConversionsMappingOption]).nonEmpty
    val allowImplicitMappings    = c.inferImplicitValue(c.universe.typeOf[DisableImplicitMappingsMappingOption]).isEmpty
    AutoMappingOptions(allowEmptyOptional, allowEmptyDefaults, allowImplicitConversions, allowImplicitMappings)
  }

  private def buildMacroImplicitUsage(c: blackbox.Context)(options: AutoMappingOptions): c.Tree = {
    import c.universe._
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

  private def extractSelector(c: blackbox.Context)(tree: c.Tree, pos: c.Position): Seq[String] = {
    import c.universe._
    tree match {
      case Ident(_)                   => Seq.empty
      case Select(subtree, fieldName) =>
        val name = fieldName.decodedName.toString
        extractSelector(c)(subtree, pos) :+ name
      case _                          =>
        c.abort(pos, s"For building mapping rule you should specify target field as _.someField.")
    }
  }

}
