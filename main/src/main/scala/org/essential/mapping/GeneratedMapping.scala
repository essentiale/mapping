package org.essential.mapping

import org.essential.mapping.auto.AutoMappingMacro
import scala.language.experimental.macros

trait GeneratedMapping[S, T] extends Mapping[S, T]

trait GeneratedRecursiveMapping[S, T] extends Mapping[S, T]

object GeneratedMapping {

  implicit def generatedMapping[S, T]: GeneratedMapping[S, T] = macro AutoMappingMacro.mapInline[S, T]

}

object GeneratedRecursiveMapping {

  implicit def generatedRecursiveMapping[S, T]: GeneratedRecursiveMapping[S, T] = macro AutoMappingMacro.mapInlineRecursive[S, T]

}
