package org.essential.test.mapping

import org.essential.mapping.Mapping
import org.essential.mapping.auto._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

/**
 * @author Konstantin Volchenko
 */
class MappingWithRulesTest extends AnyFlatSpecLike
  with BeforeAndAfter
  with BeforeAndAfterAll with Matchers {

  behavior of "Generating mapper with rules"

  it should "generate empty mapper" in {
    case class Source()
    case class Target()

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
      )
    }

    mapper.map(Source()) shouldBe Target()
  }

  it should "generate mapper with missing fields" in {
    case class Source(c: Int)
    case class Target(a: Int, b: String)

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.a)(_.c+2),
        mapping(_.b)(_ => "value")
      )
    }

    mapper.map(Source(5)) shouldBe Target(7, "value")
  }

  it should "generate mapper with missing and existing fields" in {
    case class Source(a: Int)
    case class Target(a: Int, b: String)

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_ => "value")
      )
    }

    mapper.map(Source(5)) shouldBe Target(5, "value")
  }

  it should "generate mapper with alternative target field specification" in {
    case class Source(a: Int)
    case class Target(a: Int, b: String)

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(x => x.b)(_ => "value")
      )
    }

    mapper.map(Source(5)) shouldBe Target(5, "value")
  }

  it should "not generate mapper with absent fields without rule" in {
    case class Source(a: Int)
    case class Target(a: Int, b: String, c: Boolean)


    """val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_ => "value")
      )
    }""" shouldNot compile

    """val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_ => "value"),
        mapping(_.c)(_ => true)
      )
    }""" should compile

  }

  it should "not generate mapper with duplicate field rules" in {
    case class Source(a: Int)
    case class Target(a: Int, b: String)


    """val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_ => "value")
      )
    }""" should compile

    """val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_ => "value"),
        mapping(_.b)(_ => "another")
      )
    }""" shouldNot compile

  }

  it should "not generate mapper with incompatible field types" in {
    case class Source(a: Int)
    case class Target(a: Int, b: String)


    """val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_ => 4)
      )
    }""" shouldNot compile

    """val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_ => "value")
      )
    }""" should compile

  }

  it should "not generate mapper with absent target fields" in {
    case class Source(a: Int)
    case class Target(a: Int, b: String)

    """val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.d)(_ => "value"),
        mapping(_.b)(_ => "value")
      )
    }""" shouldNot compile

    """val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_ => "value")
      )
    }""" should compile

  }

  it should "not generate mapper with illegal target fields specification" in {
    case class Source(a: Int)
    case class Target(a: Int, b: String)

    """val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_ => "asd")(_ => "value")
      )
    }""" shouldNot compile

    """val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_ => "value")
      )
    }""" should compile

  }

  behavior of "Default value"

  it should "not compile if field with default value is missing" in {
    case class IncorrectSource(d: String)

    case class CorrectSource(a: Int, d: String)

    case class Target(a: Int = 5, b: String)

    """val mapper: Mapping[CorrectSource, Target] = new AutoMapping[CorrectSource, Target] {
            val rules: CorrectSource => Target = build(
              mapping(_.b)(_.d)
            )
          }""" should compile

    """val mapper: Mapping[IncorrectSource, Target] = new AutoMapping[IncorrectSource, Target] {
            val rules: IncorrectSource => Target = build(
              mapping(_.b)(_.d)
            )
          }""" shouldNot compile
  }

  it should "map with absent field with default value if allowed" in {
    case class Source(d: String)

    case class Target(a: Int = 5, b: String)

    implicit val opt: AllowEmptyDefaultsMappingOption = AllowEmptyDefaultsMappingOption

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_.d)
      )
    }
    mapper.map(Source("Hello")) shouldBe Target(5, "Hello")
  }

  it should "map with several absent fields with default value if allowed" in {
    case class Source(bb: String, c: Int)

    case class Target(a: Int = 5, b: String, c: Int, d: Boolean = false)

    implicit val opt: AllowEmptyDefaultsMappingOption = AllowEmptyDefaultsMappingOption

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_.bb)
      )
    }
    mapper.map(Source("Hello",8)) shouldBe Target(5, "Hello", 8)
  }

  it should "map with absent field with default value if allowed via trait" in {
    case class Source(d: String)

    case class Target(a: Int = 5, b: String)

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] with AllowedEmptyDefaultsMapping {
      val rules: Source => Target = build(
        mapping(_.b)(_.d)
      )
    }
    mapper.map(Source("Hello")) shouldBe Target(5, "Hello")
  }

  behavior of "Option fields"

  it should "not compile if Option field is missing" in {
    case class IncorrectSource(d: String)

    case class CorrectSource(a: Option[Int], d: String)

    case class Target(a: Option[Int], b: String)

    """val mapper: Mapping[IncorrectSource, Target] = new AutoMapping[IncorrectSource, Target] {
            val rules: IncorrectSource => Target = build(
              mapping(_.b)(_.d)
            )
          }""" shouldNot compile
    """val mapper: Mapping[CorrectSource, Target] = new AutoMapping[CorrectSource, Target] {
            val rules: CorrectSource => Target = build(
              mapping(_.b)(_.d)
            )
          }""" should compile
  }

  it should "map with absent Option field if allowed" in {
    case class Source(d: String)

    case class Target(a: Option[Int], b: String)

    implicit val opt: AllowEmptyOptionalMappingOption = AllowEmptyOptionalMappingOption

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_.d)
      )
    }
    mapper.map(Source("Hello")) shouldBe Target(None, "Hello")
  }

  it should "map with several absent Option field if allowed" in {
    case class Source(b: String, k: Int)

    case class Target(a: Option[Int], b: String, c: Option[Boolean], d: Int)

    implicit val opt: AllowEmptyOptionalMappingOption = AllowEmptyOptionalMappingOption

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.d)(_.k)
      )
    }
    mapper.map(Source("Hello",5)) shouldBe Target(None, "Hello", None, 5)
  }

  it should "prefer default value over Option when both allowed" in {
    case class Source(b1: String)

    case class Target(a: Option[Int], b: String, c: Boolean = true, d: Option[String] = Some("Good"))

    implicit val opt1: AllowEmptyOptionalMappingOption = AllowEmptyOptionalMappingOption
    implicit val opt2: AllowEmptyDefaultsMappingOption = AllowEmptyDefaultsMappingOption

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_.b1)
      )
    }
    mapper.map(Source("Hello")) shouldBe Target(None, "Hello", c = true, Some("Good"))
  }

  it should "map with absent Option field if allowed via trait" in {
    case class Source(d: String)

    case class Target(a: Option[Int], b: String)

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] with AllowedEmptyOptionalMapping {
      val rules: Source => Target = build(
        mapping(_.b)(_.d)
      )
    }
    mapper.map(Source("Hello")) shouldBe Target(None, "Hello")
  }

  behavior of "implicit conversions"

  it should "not compile when field is assignable via implicit conversion" in {
    case class FromDef(i: Int)
    case class FromFunc(j: Int)
    case class To(k: Int)

    case class IncorrectSource1(a: FromDef, d: String)
    case class IncorrectSource2(a: FromFunc, d: String)
    case class CorrectSource(a: To, d: String)

    case class Target(a: To, b: String)

    implicit def convertFromDef(from: FromDef): To = To(from.i)
    implicit def convertFromFunc: FromFunc => To = source => To(source.j)

    // Just work around unused implicit warning
    val r1: To = FromDef(2)
    r1 shouldBe To(2)
    val r2: To = FromFunc(2)
    r2 shouldBe To(2)

    """val mapper: Mapping[IncorrectSource1, Target] = new AutoMapping[IncorrectSource1, Target] {
            val rules: IncorrectSource1 => Target = build(
              mapping(_.b)(_.d)
            )
          }""" shouldNot compile
    """val mapper: Mapping[IncorrectSource2, Target] = new AutoMapping[IncorrectSource2, Target] {
            val rules: IncorrectSource2 => Target = build(
              mapping(_.b)(_.d)
            )
          }""" shouldNot compile
    """val mapper: Mapping[CorrectSource, Target] = new AutoMapping[CorrectSource, Target] {
            val rules: CorrectSource => Target = build(
              mapping(_.b)(_.d)
            )
          }""" should compile
  }

  it should "compile when field is assignable via implicit def conversion if allowed" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class Source(a: SubSource, d: String)

    case class Target(a: SubTarget, b: String)

    implicit def convertFromDef(src: SubSource): SubTarget = SubTarget(src.i)
    implicit val opt: AllowImplicitConversionsMappingOption = AllowImplicitConversionsMappingOption

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_.d)
      )
    }
    mapper.map(Source(SubSource(6), "str")) shouldBe Target(SubTarget(6), "str")
  }

  it should "compile when field is assignable via implicit func conversion if allowed" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class Source(a: SubSource, d: String)

    case class Target(a: SubTarget, b: String)

    implicit val convertFromDef: SubSource => SubTarget = src => SubTarget(src.i)
    implicit val opt: AllowImplicitConversionsMappingOption = AllowImplicitConversionsMappingOption

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_.d)
      )
    }
    mapper.map(Source(SubSource(6), "str")) shouldBe Target(SubTarget(6), "str")
  }

  it should "compile when field is assignable via implicit def conversion if allowed via trait" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class Source(a: SubSource, d: String)

    case class Target(a: SubTarget, b: String)

    implicit def convertFromDef(src: SubSource): SubTarget = SubTarget(src.i)

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] with AllowedImplicitConversionsMapping {
      val rules: Source => Target = build(
        mapping(_.b)(_.d)
      )
    }
    mapper.map(Source(SubSource(6), "str")) shouldBe Target(SubTarget(6), "str")
  }

  behavior of "Mapping implicits"

  it should "not compile when field is assignable via implicit Mapping if disabled" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class IncorrectSource(a: SubSource, d: String)
    case class CorrectSource(a: SubTarget, d: String)

    case class Target(a: SubTarget, b: String)

    implicit val subMapping: Mapping[SubSource, SubTarget] = source => SubTarget(source.i)
    implicit val opt: DisableImplicitMappingsMappingOption = DisableImplicitMappingsMappingOption

    // Workaround unused implicit warning
    opt shouldBe DisableImplicitMappingsMappingOption
    subMapping.map(SubSource(5)) shouldBe SubTarget(5)

    """val mapper: Mapping[IncorrectSource, Target] = new AutoMapping[IncorrectSource, Target] {
            val rules: IncorrectSource => Target = build(
              mapping(_.b)(_.d)
            )
          }""" shouldNot compile
    """val mapper: Mapping[CorrectSource, Target] = new AutoMapping[CorrectSource, Target] {
            val rules: CorrectSource => Target = build(
              mapping(_.b)(_.d)
            )
          }""" should compile
  }

  it should "not compile when field is assignable via implicit Mapping if disabled via trait" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class IncorrectSource(a: SubSource, d: String)
    case class CorrectSource(a: SubTarget, d: String)

    case class Target(a: SubTarget, b: String)

    implicit val subMapping: Mapping[SubSource, SubTarget] = source => SubTarget(source.i)

    // Workaround unused implicit warning
    subMapping.map(SubSource(5)) shouldBe SubTarget(5)

    """val mapper: Mapping[IncorrectSource, Target] = new AutoMapping[IncorrectSource, Target] with org.essential.mapping.auto.DisabledImplicitMappingsMapping {
            val rules: IncorrectSource => Target = build(
              mapping(_.b)(_.d)
            )
          }""" shouldNot compile
    """val mapper: Mapping[CorrectSource, Target] = new AutoMapping[CorrectSource, Target] with org.essential.mapping.auto.DisabledImplicitMappingsMapping {
            val rules: CorrectSource => Target = build(
              mapping(_.b)(_.d)
            )
          }""" should compile
  }

  it should "compile when field is assignable via implicit Mapping if allowed" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class Source(a: SubSource, d: String)

    case class Target(a: SubTarget, b: String)

    implicit val subMapping: Mapping[SubSource, SubTarget] = source => SubTarget(source.i)

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_.d)
      )
    }
    mapper.map(Source(SubSource(2), "str")) shouldBe Target(SubTarget(2), "str")
  }

  it should "prefer implicit Mapping over implicit conversion when both allowed" in {
    case class SubSource(i: Int)
    case class SubTarget(k: Int)

    case class Source(a: SubSource, d: String)

    case class Target(a: SubTarget, b: String)

    implicit val subMapping: Mapping[SubSource, SubTarget] = source => SubTarget(source.i+10)
    implicit val subConversion: SubSource => SubTarget = source => SubTarget(source.i)
    implicit def subConvert(src: SubSource): SubTarget = SubTarget(src.i)

    implicit val opt: AllowImplicitConversionsMappingOption = AllowImplicitConversionsMappingOption

    // Workaround unused implicit warning
    subConversion(SubSource(5)) shouldBe SubTarget(5)
    subConvert(SubSource(5)) shouldBe SubTarget(5)

    val mapper: Mapping[Source, Target] = new AutoMapping[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(_.d)
      )
    }
    mapper.map(Source(SubSource(2), "str")) shouldBe Target(SubTarget(12), "str")
  }

}
