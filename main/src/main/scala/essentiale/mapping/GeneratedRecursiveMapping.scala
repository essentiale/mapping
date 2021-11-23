package essentiale.mapping

import essentiale.mapping.auto.AutoMappingMacro

import scala.language.experimental.macros

/**
 * @author Konstantin Volchenko
 */
trait GeneratedRecursiveMapping[S, T] {
  def map(source: S): T
}

object GeneratedRecursiveMapping {

  implicit def generatedRecursiveMapping[S, T]: GeneratedRecursiveMapping[S, T] = macro AutoMappingMacro.mapInlineRecursive[S, T]

}
