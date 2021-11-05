package essentiale.test.mapping

import essentiale.mapping.Mapping
import essentiale.mapping.auto._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

/**
 * @author Konstantin Volchenko
 */
class GeneratedMappingTest extends AnyFlatSpecLike with BeforeAndAfter with BeforeAndAfterAll with Matchers {

  behavior of "Default auto mapping"

  it should "map empty case class" in {

    case class Source()

    case class Target()

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source()) shouldBe Target()
  }

  it should "map one field case class" in {

    case class Source(a: Int)

    case class Target(a: Int)

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source(4)) shouldBe Target(4)
  }

  it should "map two field case class" in {

    case class Source(a: Int, b: String, c: Boolean)

    case class Target(a: Int, b: String)

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source(2, "str", c = true)) shouldBe Target(2, "str")
  }

  it should "map two field case class in mixed order" in {

    case class Source(c: Boolean, b: String, a: Int)

    case class Target(a: Int, b: String)

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source(c = true, "str", 2)) shouldBe Target(2, "str")
  }

  it should "map assignable but not equal types" in {

    trait Base

    case class Child(age: Int) extends Base

    case class Source(person: Child, b: String)

    case class Target(person: Base, b: String)

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source(Child(8), "str")) shouldBe Target(Child(8), "str")
  }

  it should "not compile when field is missing" in {

    case class IncompleteSource(b: String)

    case class CompleteSource(a: Int, b: String)

    case class Target(a: Int, b: String)

    "val mapping = AutoMapping.generate[IncompleteSource, Target]" shouldNot compile
    "val mapping = AutoMapping.generate[CompleteSource, Target]" should compile
  }

  it should "not compile when field is incompatible" in {

    case class IncorrectSource(a: Boolean, b: String)

    case class CorrectSource(a: Int, b: String)

    case class Target(a: Int, b: String)

    "val mapping = AutoMapping.generate[IncorrectSource, Target]" shouldNot compile
    "val mapping = AutoMapping.generate[CorrectSource, Target]" should compile
  }

  behavior of "Default values"

  it should "not compile if field with default value is missing" in {
    case class IncorrectSource(b: String)

    case class CorrectSource(a: Int, b: String)

    case class Target(a: Int = 5, b: String)

    "val mapping = AutoMapping.generate[IncorrectSource, Target]" shouldNot compile
    "val mapping = AutoMapping.generate[CorrectSource, Target]" should compile
  }

  it should "map with absent field with default value if allowed" in {
    case class Source(b: String)

    case class Target(a: Int = 5, b: String)

    implicit val opt: AllowEmptyDefaultsMappingOption = AllowEmptyDefaultsMappingOption

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source("Hello")) shouldBe Target(5, "Hello")
  }

  it should "map with several absent fields with default value if allowed" in {
    case class Source(b: String, c: Int)

    case class Target(a: Int = 5, b: String, c: Int, d: Boolean = false)

    implicit val opt: AllowEmptyDefaultsMappingOption = AllowEmptyDefaultsMappingOption

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source("Hello", 8)) shouldBe Target(5, "Hello", 8)
  }

  behavior of "Option fields"

  it should "not compile if Option field is missing" in {
    case class IncorrectSource(b: String)

    case class CorrectSource(a: Option[Int], b: String)

    case class Target(a: Option[Int], b: String)

    "val mapping = AutoMapping.generate[IncorrectSource, Target]" shouldNot compile
    "val mapping = AutoMapping.generate[CorrectSource, Target]" should compile
  }

  it should "map with absent Option field if allowed" in {
    case class Source(b: String)

    case class Target(a: Option[Int], b: String)

    implicit val opt: AllowEmptyOptionalMappingOption = AllowEmptyOptionalMappingOption

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source("Hello")) shouldBe Target(None, "Hello")
  }

  it should "map with several absent Option field if allowed" in {
    case class Source(b: String, d: Int)

    case class Target(a: Option[Int], b: String, c: Option[Boolean], d: Int)

    implicit val opt: AllowEmptyOptionalMappingOption = AllowEmptyOptionalMappingOption

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source("Hello", 5)) shouldBe Target(None, "Hello", None, 5)
  }

  it should "prefer default value over Option when both allowed" in {
    case class Source(b: String)

    case class Target(a: Option[Int], b: String, c: Boolean = true, d: Option[String] = Some("Good"))

    implicit val opt1: AllowEmptyOptionalMappingOption = AllowEmptyOptionalMappingOption
    implicit val opt2: AllowEmptyDefaultsMappingOption = AllowEmptyDefaultsMappingOption

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source("Hello")) shouldBe Target(None, "Hello", c = true, Some("Good"))
  }

  behavior of "implicit conversions"

  it should "not compile when field is assignable via implicit conversion" in {
    case class FromDef(i: Int)
    case class FromFunc(j: Int)
    case class To(k: Int)

    case class IncorrectSource1(a: FromDef, b: String)
    case class IncorrectSource2(a: FromFunc, b: String)
    case class CorrectSource(a: To, b: String)

    case class Target(a: To, b: String)

    implicit def convertFromDef(from: FromDef): To = To(from.i)
    implicit def convertFromFunc: FromFunc => To   = source => To(source.j)

    // Just work around unused implicit warning
    val r1: To = FromDef(2)
    r1 shouldBe To(2)
    val r2: To = FromFunc(2)
    r2 shouldBe To(2)

    "val mapping = AutoMapping.generate[IncorrectSource1, Target]" shouldNot compile
    "val mapping = AutoMapping.generate[IncorrectSource2, Target]" shouldNot compile
    "val mapping = AutoMapping.generate[CorrectSource, Target]" should compile
  }

  it should "compile when field is assignable via implicit def conversion if allowed" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class Source(a: SubSource, b: String)

    case class Target(a: SubTarget, b: String)

    implicit def convertFromDef(src: SubSource): SubTarget  = SubTarget(src.i)
    implicit val opt: AllowImplicitConversionsMappingOption = AllowImplicitConversionsMappingOption

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source(SubSource(6), "str")) shouldBe Target(SubTarget(6), "str")
  }

  it should "compile when field is assignable via implicit func conversion if allowed" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class Source(a: SubSource, b: String)

    case class Target(a: SubTarget, b: String)

    implicit val convertFromDef: SubSource => SubTarget     = src => SubTarget(src.i)
    implicit val opt: AllowImplicitConversionsMappingOption = AllowImplicitConversionsMappingOption

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source(SubSource(6), "str")) shouldBe Target(SubTarget(6), "str")
  }

  behavior of "Mapping implicits"

  it should "not compile when field is assignable via implicit Mapping if disabled" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class IncorrectSource(a: SubSource, b: String)
    case class CorrectSource(a: SubTarget, b: String)

    case class Target(a: SubTarget, b: String)

    implicit val subMapping: Mapping[SubSource, SubTarget] = source => SubTarget(source.i)
    implicit val opt: DisableImplicitMappingsMappingOption = DisableImplicitMappingsMappingOption

    // Workaround unused implicit warning
    opt shouldBe DisableImplicitMappingsMappingOption
    subMapping.map(SubSource(5)) shouldBe SubTarget(5)

    "val mapping = AutoMapping.generate[IncorrectSource, Target]" shouldNot compile
    "val mapping = AutoMapping.generate[CorrectSource, Target]" should compile
  }

  it should "compile when field is assignable via implicit Mapping if allowed" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class Source(a: SubSource, b: String)

    case class Target(a: SubTarget, b: String)

    implicit val subMapping: Mapping[SubSource, SubTarget] = source => SubTarget(source.i)

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source(SubSource(2), "str")) shouldBe Target(SubTarget(2), "str")
  }

  it should "prefer implicit Mapping over implicit conversion when both allowed" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class Source(a: SubSource, b: String)

    case class Target(a: SubTarget, b: String)

    implicit val subMapping: Mapping[SubSource, SubTarget] = source => SubTarget(source.i + 10)
    implicit val subConversion: SubSource => SubTarget     = source => SubTarget(source.i)
    implicit def subConvert(src: SubSource): SubTarget     = SubTarget(src.i)

    implicit val opt: AllowImplicitConversionsMappingOption = AllowImplicitConversionsMappingOption

    // Workaround unused implicit warning
    subConversion(SubSource(5)) shouldBe SubTarget(5)
    subConvert(SubSource(5)) shouldBe SubTarget(5)

    val mapping = AutoMapping.generate[Source, Target]
    mapping.map(Source(SubSource(2), "str")) shouldBe Target(SubTarget(12), "str")
  }

}
