package org.essential.test.mapping

import org.essential.mapping.Mapping
import org.essential.mapping.auto._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers


/**
 * @author Konstantin Volchenko
 */
class RecursiveMappingTest extends AnyFlatSpecLike
  with BeforeAndAfter
  with BeforeAndAfterAll with Matchers {

  behavior of "Recursive generated mapping"

  it should "Generate recursive mapping" in {
    case class Source(a: Int, b: SubSource1, c: SubSource2, d: Boolean)
    case class Target(c: SubTarget2, a: Int, b: SubTarget1)

    case class SubSource1(x: String, y: Int)
    case class SubTarget1(y: Int, x: String)

    case class SubSource2(inner: SubSubSource)
    case class SubTarget2(inner: SubSubTarget)

    case class SubSubSource(i: Int)
    case class SubSubTarget(i: Int)

    val mapping = AutoMapping.generateRecursive[Source, Target]
    mapping.map(Source(5, SubSource1("str", 2), SubSource2(SubSubSource(9)), d = true)) shouldBe Target(
      SubTarget2(SubSubTarget(9)),
      5,
      SubTarget1(2, "str")
    )
  }

  it should "not Generate recursive mapping by non recursive method" in {
    case class Source(a: Int, b: SubSource1, c: SubSource2, d: Boolean)
    case class Target(c: SubTarget2, a: Int, b: SubTarget1)

    case class SubSource1(x: String, y: Int)
    case class SubTarget1(y: Int, x: String)

    case class SubSource2(inner: SubSubSource)
    case class SubTarget2(inner: SubSubTarget)

    case class SubSubSource(i: Int)
    case class SubSubTarget(i: Int)

    "val mapping = AutoMapping.generateRecursive[Source, Target]" should compile
    "val mapping = AutoMapping.generate[Source, Target]" shouldNot compile
  }

  it should "Generate recursive mapping inline" in {
    case class Source(a: Int, b: SubSource1, c: SubSource2, d: Boolean)
    case class Target(c: SubTarget2, a: Int, b: SubTarget1)

    case class SubSource1(x: String, y: Int)
    case class SubTarget1(y: Int, x: String)

    case class SubSource2(inner: SubSubSource)
    case class SubTarget2(inner: SubSubTarget)

    case class SubSubSource(i: Int)
    case class SubSubTarget(i: Int)

    AutoMapping.map(Source(5, SubSource1("str", 2), SubSource2(SubSubSource(9)), d = true)).recursiveTo[Target] shouldBe Target(
      SubTarget2(SubSubTarget(9)),
      5,
      SubTarget1(2, "str")
    )
  }

  it should "not generate recursive mapping inline with non recursive method" in {
    case class Source(a: Int, b: SubSource1, c: SubSource2, d: Boolean)
    case class Target(c: SubTarget2, a: Int, b: SubTarget1)

    case class SubSource1(x: String, y: Int)
    case class SubTarget1(y: Int, x: String)

    case class SubSource2(inner: SubSubSource)
    case class SubTarget2(inner: SubSubTarget)

    case class SubSubSource(i: Int)
    case class SubSubTarget(i: Int)

    """AutoMapping.map(Source(5, SubSource1("str", 2), SubSource2(SubSubSource(9)), d = true)).recursiveTo[Target]""" should compile
    """AutoMapping.map(Source(5, SubSource1("str", 2), SubSource2(SubSubSource(9)), d = true)).to[Target]""" shouldNot compile
  }

  it should "Generate mapping with rules recursively" in {
    case class Source(a: Int, b: SubSource1, c: SubSource2)
    case class Target(c: SubTarget2, a: Int, b: SubTarget1, d: Boolean)

    case class SubSource1(x: String, y: Int)
    case class SubTarget1(y: Int, z: String)

    case class SubSource2(inner: SubSubSource)
    case class SubTarget2(inner: SubSubTarget, a: Int)

    case class SubSubSource(i: Int)
    case class SubSubTarget(i: Int, b: Int, c: Int)

    val mapper: Mapping[Source, Target] = new AutoMappingRecursive[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.c.inner.b)(_ => 2),
        mapping(_.c.inner.c)(_.c.inner.i - 2),
        mapping(_.c.a)(_.a),
        mapping(_.b.z)(_.b.x),
        mapping(_.d)(x => x.b.y < x.c.inner.i)
      )
    }

    mapper.map(Source(2, SubSource1("a", 7), SubSource2(SubSubSource(9)))) shouldBe Target(
      SubTarget2(SubSubTarget(9, 2, 7), 2),
      2,
      SubTarget1(7, "a"),
      d = true
    )
  }

  it should "mapping rule should have precedence over recursive conversion" in {
    case class Source(a: Int, b: SubSource)
    case class Target(a: Int, b: SubTarget)

    case class SubSource(x: String, y: Int)
    case class SubTarget(y: Int, x: String)

    val mapper1: Mapping[Source, Target] = new AutoMappingRecursive[Source, Target] {
      val rules: Source => Target = build(
        mapping(_.b)(x => SubTarget(x.b.y, "Good"))
      )
    }

    val mapper2: Mapping[Source, Target] = new AutoMappingRecursive[Source, Target] {
      val rules: Source => Target = build(
      )
    }

    mapper1.map(Source(2, SubSource("Bad",7))) shouldBe Target(2, SubTarget(7, "Good"))
    mapper2.map(Source(2, SubSource("Bad",7))) shouldBe Target(2, SubTarget(7, "Bad"))

  }

}
