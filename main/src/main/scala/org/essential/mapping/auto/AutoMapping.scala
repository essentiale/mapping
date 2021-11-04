package org.essential.mapping.auto

import org.essential.mapping.{GeneratedMapping, GeneratedRecursiveMapping, Mapping}
import scala.language.experimental.macros

/**
 * @author Konstantin Volchenko
 */
trait AutoMapping[S, T] extends Mapping[S, T] with AutoMappingHelper {

  override def map(source: S): T = rules(source)

  val rules: S => T

  protected def mapping[V](target: T => V)(source: S => V): MappingRuleAssignment[S, T, V] = MappingRuleAssignment(source)

  protected def build(rules: MappingRule[S, T]*): S => T = macro AutoMappingMacro.buildMappingWithRules[S, T]

}

trait AutoMappingRecursive[S, T] extends Mapping[S, T] with AutoMappingHelper {

  override def map(source: S): T = rules(source)

  val rules: S => T

  protected def mapping[V](target: T => V)(source: S => V): MappingRuleAssignment[S, T, V] = MappingRuleAssignment(source)

  protected def build(rules: MappingRule[S, T]*): S => T = macro AutoMappingMacro.buildMappingRecusiveWithRules[S, T]

}

trait AutoMappingHelper {

  protected def useAllowEmptyOptionsMappingOption(implicit opt: AllowEmptyOptionalMappingOption): Unit = {}
  protected def useAllowEmptyDefaultsMappingOption(implicit opt: AllowEmptyDefaultsMappingOption): Unit = {}
  protected def useAllowImplicitConversionsMappingOption(implicit opt: AllowImplicitConversionsMappingOption): Unit = {}
  protected def useDisableImplicitMappingsMappingOption(implicit opt: DisableImplicitMappingsMappingOption): Unit = {}

}

private[mapping] class PartialMapping[S](val source: S) {
  def to[T](implicit m: GeneratedMapping[S, T]): T                   = m.map(source)
  def recursiveTo[T](implicit m: GeneratedRecursiveMapping[S, T]): T = m.map(source)
}

object AutoMapping {

  def generate[S, T]: Mapping[S, T] = macro AutoMappingMacro.buildMapping[S, T]

  def generateRecursive[S, T]: Mapping[S, T] = macro AutoMappingMacro.buildMappingRecursive[S, T]

  def map[S](source: S): PartialMapping[S] = new PartialMapping[S](source)

}
