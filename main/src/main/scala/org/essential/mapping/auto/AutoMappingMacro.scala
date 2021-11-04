package org.essential.mapping.auto

import org.essential.mapping.{GeneratedMapping, GeneratedRecursiveMapping, Mapping}
import scala.collection.mutable
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
            protected def useAllowEmptyOptionsMappingOption(implicit opt: ${typeOf[AllowEmptyOptionalMappingOption]}): Unit = {}
            protected def useAllowEmptyDefaultsMappingOption(implicit opt: ${typeOf[AllowEmptyDefaultsMappingOption]}): Unit = {}
            protected def useAllowImplicitConversionsMappingOption(implicit opt: ${typeOf[AllowImplicitConversionsMappingOption]}): Unit = {}
            protected def useDisableImplicitMappingsMappingOption(implicit opt: ${typeOf[DisableImplicitMappingsMappingOption]}): Unit = {}
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
            protected def useAllowEmptyOptionsMappingOption(implicit opt: ${typeOf[AllowEmptyOptionalMappingOption]}): Unit = {}
            protected def useAllowEmptyDefaultsMappingOption(implicit opt: ${typeOf[AllowEmptyDefaultsMappingOption]}): Unit = {}
            protected def useAllowImplicitConversionsMappingOption(implicit opt: ${typeOf[AllowImplicitConversionsMappingOption]}): Unit = {}
            protected def useDisableImplicitMappingsMappingOption(implicit opt: ${typeOf[DisableImplicitMappingsMappingOption]}): Unit = {}
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
            protected def useAllowEmptyOptionsMappingOption(implicit opt: ${typeOf[AllowEmptyOptionalMappingOption]}): Unit = {}
            protected def useAllowEmptyDefaultsMappingOption(implicit opt: ${typeOf[AllowEmptyDefaultsMappingOption]}): Unit = {}
            protected def useAllowImplicitConversionsMappingOption(implicit opt: ${typeOf[AllowImplicitConversionsMappingOption]}): Unit = {}
            protected def useDisableImplicitMappingsMappingOption(implicit opt: ${typeOf[DisableImplicitMappingsMappingOption]}): Unit = {}
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
            case _ =>
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

  private def extractSelector(c: blackbox.Context)(tree: c.Tree, pos: c.Position): Seq[String] = {
    import c.universe._
    tree match {
      case Ident(_)                   => Seq.empty
      case Select(subtree, fieldName) =>
        val name = fieldName.decodedName.toString
        extractSelector(c)(subtree, pos) :+ name
      case _                          =>
        c.abort(pos, s"For building mapping rule you should specify target field as _.someField")
    }
  }

  private def buildMappingInternal[S: c.WeakTypeTag, T: c.WeakTypeTag](
    c: blackbox.Context
  )(mappings: Map[String, c.Tree], sourceVariableName: c.Tree, recursive: Boolean): c.Tree = {
    import c.universe._
    val unusedRules = mutable.Set.from(mappings.keySet)
    val options = readMappingOptions(c)
    val mapping = buildClassMapping(c)(weakTypeOf[S], weakTypeOf[T], mappings, sourceVariableName, options, recursive, Seq.empty, unusedRules)
    for (rule <- unusedRules) {
      c.warning(mappings(rule).pos, s"Mapping rule for $rule field is not used.")
    }
    val result  = q"""
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
    targetPrefix: Seq[String],
    unusedRules: mutable.Set[String]
  ): c.Tree = {
    import c.universe._
    val targetConstructor = getCaseClassConstructor(c)(toTpe)
    val args              = targetConstructor.paramLists.head.flatMap(
      makeArgument(c)(_, mappings, options, fromTpe, sourceVariableName, recursive, targetPrefix, unusedRules)
    )
    val result            = q"new ${toTpe}(..$args)"
    result
  }

  private def buildMacroImplicitUsage(c: blackbox.Context)(options: AutoMappingOptions): c.Tree = {
    import c.universe._
    val opt1 =
      if (options.allowEmptyOptional)
        q"useAllowEmptyOptionsMappingOption"
      else
        EmptyTree
    val opt2 =
      if (options.allowEmptyDefaults)
        q"useAllowEmptyDefaultsMappingOption"
      else
        EmptyTree
    val opt3 =
      if (options.allowImplicitConversions)
        q"useAllowImplicitConversionsMappingOption"
      else
        EmptyTree
    val opt4 =
      if (!options.allowImplicitMappings)
        q"useDisableImplicitMappingsMappingOption"
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

  private def makeArgument(c: blackbox.Context)(
    arg: c.Symbol,
    mappings: Map[String, c.Tree],
    options: AutoMappingOptions,
    sourceType: c.Type,
    sourceVariableName: c.Tree,
    recursive: Boolean,
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
            val fromTpe         = value.asMethod.returnType
            val toTpe           = arg.typeSignature
            val implicitMapping = findImplicitMapping(c)(fromTpe, toTpe, options)
            if (implicitMapping.nonEmpty) {
              Some(
                q"${TermName(arg.name.decodedName.toString)} = implicitly[${appliedType(typeOf[Mapping[_, _]], fromTpe, toTpe)}].map($sourceVariableName.$value)"
              )
            } else if (isAssignable(c)(fromTpe, toTpe, options))
              Some(q"${TermName(arg.name.decodedName.toString)} = $sourceVariableName.$value")
            else if (recursive && fromTpe.typeSymbol.isClass && toTpe.typeSymbol.isClass && toTpe.typeSymbol.asClass.isCaseClass) {
              Some(
                buildClassMapping(c)(
                  fromTpe,
                  toTpe,
                  mappings,
                  q"$sourceVariableName.$value",
                  options,
                  recursive,
                  targetPrefix :+ arg.name.decodedName.toString,
                  unusedRules
                )
              )
            } else {
              c.abort(
                c.enclosingPosition,
                s"Could not convert value of field $fieldName from source type ${value.asMethod.returnType} to target type ${arg.typeSignature}"
              )
            }
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

}
