package essentiale.test.mapping

import essentiale.mapping.Mapping
import essentiale.mapping.auto._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

/**
 * @author Konstantin Volchenko
 */
class RecursiveMappingTest extends AnyFlatSpecLike with BeforeAndAfter with BeforeAndAfterAll with Matchers {

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

    mapper1.map(Source(2, SubSource("Bad", 7))) shouldBe Target(2, SubTarget(7, "Good"))
    mapper2.map(Source(2, SubSource("Bad", 7))) shouldBe Target(2, SubTarget(7, "Bad"))

  }

  it should "Generate recursive mapping including Option[_]" in {
    case class Source(a: Int, b: SubSource1, c: Option[SubSource2], d: Boolean)
    case class Target(c: Option[SubTarget2], a: Int, b: SubTarget1)

    case class SubSource1(x: String, y: Option[Int])
    case class SubTarget1(y: Option[Int], x: String)

    case class SubSource2(inner: Option[Option[SubSubSource]])
    case class SubTarget2(inner: Option[Option[SubSubTarget]])

    case class SubSubSource(i: Int)
    case class SubSubTarget(i: Int)

    val mapping = AutoMapping.generateRecursive[Source, Target]
    mapping.map(Source(5, SubSource1("str", Some(2)), Some(SubSource2(Some(Some(SubSubSource(9))))), d = true)) shouldBe Target(
      Some(SubTarget2(Some(Some(SubSubTarget(9))))),
      5,
      SubTarget1(Some(2), "str")
    )
  }

  it should "Generate recursive mapping including Seq[_]" in {
    case class Source(a: Int, b: Seq[Int], c: Seq[SubSource1], d: Boolean)
    case class Target(c: Seq[SubTarget1], a: Int, b: Seq[Int])

    case class SubSource1(x: String, y: Int)
    case class SubTarget1(y: Int, x: String)

    val mapping = AutoMapping.generateRecursive[Source, Target]
    mapping.map(Source(5, Seq(2, 3, 4), Seq(SubSource1("str", 2), SubSource1("x", 4)), d = true)) shouldBe Target(
      Seq(SubTarget1(2, "str"), SubTarget1(4, "x")),
      5,
      Seq(2, 3, 4)
    )
  }

  it should "Generate recursive mapping including Array[_]" in {
    case class Source(a: Int, b: Array[Int], c: Array[SubSource1], d: Boolean)
    case class Target(c: Array[SubTarget1], a: Int, b: Array[Int])

    case class SubSource1(x: String, y: Int)
    case class SubTarget1(y: Int, x: String)

    val mapping = AutoMapping.generateRecursive[Source, Target]
    val result  = mapping.map(Source(5, Array(2, 3, 4), Array(SubSource1("str", 2), SubSource1("x", 4)), d = true))
    result.a shouldBe 5
    result.b.toSeq shouldBe Seq(2, 3, 4)
    result.c.toSeq shouldBe Seq(SubTarget1(2, "str"), SubTarget1(4, "x"))
  }

  it should "Generate recursive mapping including Map[_]" in {
    case class Source(a: Int, b: Map[Int, SubSource1], c: Map[SubSource1, String], d: Boolean)
    case class Target(c: Map[SubTarget1, String], a: Int, b: Map[Int, SubTarget1])

    case class SubSource1(x: String, y: Int)
    case class SubTarget1(y: Int, x: String)

    val mapping = AutoMapping.generateRecursive[Source, Target]
    mapping.map(
      Source(5, Map(2 -> SubSource1("a", 0)), Map(SubSource1("str", 2) -> "r", SubSource1("x", 4) -> "t"), d = true)
    ) shouldBe Target(
      Map(SubTarget1(2, "str") -> "r", SubTarget1(4, "x") -> "t"),
      5,
      Map(2                    -> SubTarget1(0, "a"))
    )
  }

  it should "Generate recursive mapping including Either[_]" in {
    case class Source(a: Int, b: Either[Int, SubSource1], c: Either[SubSource2, SubSource3], d: Boolean)
    case class Target(c: Either[SubTarget2, SubTarget3], a: Int, b: Either[Int, SubTarget1])

    case class SubSource1(x: String, y: Int)
    case class SubSource2(x: String, y: Int)
    case class SubSource3(x: String, y: Int)
    case class SubTarget1(y: Int, x: String)
    case class SubTarget2(y: Int, x: String)
    case class SubTarget3(y: Int, x: String)

    val mapping = AutoMapping.generateRecursive[Source, Target]
    mapping.map(
      Source(5, Left(3), Right(SubSource3("str", 2)), d = true)
    ) shouldBe Target(
      Right(SubTarget3(2, "str")),
      5,
      Left(3)
    )
  }

}
