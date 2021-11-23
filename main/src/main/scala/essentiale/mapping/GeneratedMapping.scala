package essentiale.mapping

import essentiale.mapping.auto.AutoMappingMacro

import scala.language.experimental.macros

/**
 * @author Konstantin Volchenko
 */

trait GeneratedMapping[S, T] {
  def map(source: S): T
}

object GeneratedMapping {

  implicit def generatedMapping[S, T]: GeneratedMapping[S, T] = macro AutoMappingMacro.mapInline[S, T]

}
