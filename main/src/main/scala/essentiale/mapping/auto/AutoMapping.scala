package essentiale.mapping.auto

import essentiale.mapping.{GeneratedMapping, GeneratedRecursiveMapping, Mapping}

import scala.language.experimental.macros

/**
 * @author Konstantin Volchenko
 */

/**
 * Trait to extend to generate mapping from <code>S</code> class to <code>T</code> class with using of custom rules.
 * Class <code>T</code> should be a case class.
 *
 * To complete this trait you should define val <code>rules</code> like this:
 * <code>
 *   val rules: S => T = build(
 *        mapping(_.someField)(_.specialField),
 *        mapping(_.anotherField)(_ => "Special value")
 *      )
 * </code>
 * Target field should always be specified if form _.nameOfField. Source expression could be any function from S to target field type.
 *
 * Default generation of fields not specified in <code>rules</code> make by searching field in source class with the same name and
 * assignment compatible type. Fields with types that could be converted to target field type via Mapping implicit is also acceptable
 * by default. But it is possible to override this behavior by mixing special traits to your mapper:
 *
 * <ul>
 * <li><code>AllowedEmptyOptionalMapping</code> - Allow Option field in target class without corresponding field in source class. It will be filled with None value</li>
 * <li><code>AllowedEmptyDefaultsMapping</code> - Allow field with default value in target class without corresponding field in source class. It will be filled with default value</li>
 * <li><code>AllowImplicitConversionsMappingOption</code> - Allow conversion of source field type to target field type by implicit conversions</li>
 * <li><code>DisabledImplicitMappingsMapping</code> - Disallow conversion of source field type to target field type by Mapping implicit</li>
 * </ul>
 *
 * Mapping with this options could be defined like this:
 * <code>
 *   val mapping = new AutoMapping[Source, Target] with AllowedEmptyOptionalMapping {
 *      val rules: Source => Target = build(
 *        mapping(_.a)(_.b)
 *      )
 *    }
 * </code>
 *
 * Another way of change generation behavior is to define corresponding implicits in scope of rule definition. Previous example could be rewritten as:
 * <code>
 *   implicit val option: AllowEmptyOptionalMappingOption = AllowEmptyOptionalMappingOption
 *   val mapping = new AutoMapping[Source, Target] {
 *      val rules: Source => Target = build(
 *        mapping(_.a)(_.b)
 *      )
 *    }
 * </code>
 */
trait AutoMapping[S, T] extends Mapping[S, T] with AutoMappingHelper {

  /**
   * Map source class to target.
   *
   * @param source source class.
   * @return target class.
   */
  override def map(source: S): T = rules(source)

  /**
   * val field that should be implemented to generate mapping.
   * Implemented field should always be in form:
   * <code> build(mapping(..)(..),mapping(..)(..), ...)</code>
   */
  val rules: S => T

  protected def mapping[V](target: T => V)(source: S => V): MappingRuleAssignment[S, T, V] = MappingRuleAssignment(source)

  protected def build(rules: MappingRule[S, T]*): S => T = macro AutoMappingMacro.buildMappingWithRules[S, T]

}

/**
 * Trait to extend to generate mapping from <code>S</code> class to <code>T</code> class with using of custom rules.
 * Class <code>T</code> should be a case class.
 * Unlike <code>AutoMapping<code> this trait could generate mapping recursively for case classes included other case classes field that need mapping.
 *
 * To complete this trait you should define val <code>rules</code> like this:
 * <code>
 *   val rules: S => T = build(
 *        mapping(_.someField)(_.specialField),
 *        mapping(_.subClass.subField)(_ => "Special value")
 *      )
 * </code>
 * Target field should always be specified if form _.nameOfField. For fields in nested case classes full path should be used. Source expression could be any function from S to target field type.
 *
 * Default generation of fields not specified in <code>rules</code> make by searching field in source class with the same name and
 * assignment compatible type. Fields with types that could be converted to target field type via Mapping implicit is also acceptable
 * by default. But it is possible to override this behavior by mixing special traits to your mapper:
 *
 * <ul>
 * <li><code>AllowedEmptyOptionalMapping</code> - Allow Option field in target class without corresponding field in source class. It will be filled with None value</li>
 * <li><code>AllowedEmptyDefaultsMapping</code> - Allow field with default value in target class without corresponding field in source class. It will be filled with default value</li>
 * <li><code>AllowImplicitConversionsMappingOption</code> - Allow conversion of source field type to target field type by implicit conversions</li>
 * <li><code>DisabledImplicitMappingsMapping</code> - Disallow conversion of source field type to target field type by Mapping implicit</li>
 * </ul>
 *
 * Mapping with this options could be defined like this:
 * <code>
 *   val mapping = new AutoMappingRecursive[Source, Target] with AllowedEmptyOptionalMapping {
 *      val rules: Source => Target = build(
 *        mapping(_.a)(_.b)
 *      )
 *    }
 * </code>
 *
 * Another way of change generation behavior is to define corresponding implicits in scope of rule definition. Previous example could be rewritten as:
 * <code>
 *   implicit val option: AllowEmptyOptionalMappingOption = AllowEmptyOptionalMappingOption
 *   val mapping =  AutoMappingRecursive[Source, Target] {
 *      val rules: Source => Target = build(
 *        mapping(_.a)(_.b)
 *      )
 *    }
 * </code>
 */
trait AutoMappingRecursive[S, T] extends Mapping[S, T] with AutoMappingHelper {

  /**
   * Map source class to target.
   *
   * @param source source class.
   * @return target class.
   */
  override def map(source: S): T = rules(source)

  /**
   * val field that should be implemented to generate mapping.
   * Implemented field should always be in form:
   * <code> build(mapping(..)(..),mapping(..)(..), ...)</code>
   */
  val rules: S => T

  protected def mapping[V](target: T => V)(source: S => V): MappingRuleAssignment[S, T, V] = MappingRuleAssignment(source)

  protected def build(rules: MappingRule[S, T]*): S => T = macro AutoMappingMacro.buildMappingRecursiveWithRules[S, T]

}

trait AutoMappingHelper {

  protected def useAllowEmptyOptionsMappingOption(implicit opt: AllowEmptyOptionalMappingOption): Boolean              = true
  protected def useAllowEmptyDefaultsMappingOption(implicit opt: AllowEmptyDefaultsMappingOption): Boolean             = true
  protected def useAllowImplicitConversionsMappingOption(implicit opt: AllowImplicitConversionsMappingOption): Boolean = true
  protected def useDisableImplicitMappingsMappingOption(implicit opt: DisableImplicitMappingsMappingOption): Boolean   = true

}

/**
 * Intermediate class to autogenerate mappings from class <code>S</code>.
 *
 * @param source instance of source class
 */
private[mapping] class PartialMapping[S](val source: S) {

  /**
   * Automatically maps source class to target case class <code>T</code>
   *
   * @return target instance
   */
  def to[T](implicit m: GeneratedMapping[S, T]): T = m.map(source)

  /**
   * Automatically maps source class to target case class <code>T</code>
   * Unlike method <code>to[T]</code> it could map classes with nested classes if it is possible to generate mapping for them
   *
   * @return target instance
   */
  def recursiveTo[T](implicit m: GeneratedRecursiveMapping[S, T]): T = m.map(source)
}

object AutoMapping {

  /**
   * Method that automatically generates mapping from source class <code>S</code> to target case class <code>T</code>
   * @return generated mapping
   */
  def generate[S, T]: Mapping[S, T] = macro AutoMappingMacro.buildMapping[S, T]

  /**
   * Method that automatically generates mapping from source class <code>S</code> to target case class <code>T</code>
   * Unlike method <code>generate[S, T]</code> it could map classes with nested classes if it is possible to generate mapping for them
   * @return generated mapping
   */
  def generateRecursive[S, T]: Mapping[S, T] = macro AutoMappingMacro.buildMappingRecursive[S, T]

  /**
   * Generate mapping from source class <code>S</code>. Target class specified by methods <code>to[T]</code> or <code>recursiveTo[T]</code>
   * @param source instance of
   * @return intermediate class for specifying target class
   */
  def map[S](source: S): PartialMapping[S] = new PartialMapping[S](source)

}
