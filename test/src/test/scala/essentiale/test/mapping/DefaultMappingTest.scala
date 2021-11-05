package essentiale.test.mapping

import essentiale.mapping.{DefaultMappings, Mapping}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}

import scala.collection.immutable.TreeMap

/**
 * @author Konstantin Volchenko
 */
class DefaultMappingTest extends AnyFlatSpecLike with BeforeAndAfter with BeforeAndAfterAll with Matchers with DefaultMappings {

  behavior of "Default mappings"

  case class A(field: String)
  case class B(field: String)
  implicit val baseMapping: Mapping[A, B] = source => B(source.field)

  it should "convert to itself" in {
    Mapping[A, A].map(A("Hello")) shouldBe A("Hello")
  }

  it should "convert Option[_]" in {
    Mapping[Option[A], Option[B]].map(None) shouldBe None
    Mapping[Option[A], Option[B]].map(Some(A("Hello"))) shouldBe Some(B("Hello"))
  }

  it should "convert Seq[_]" in {
    Mapping[Seq[A], Seq[B]].map(Seq(A("Hello"))) shouldBe Seq(B("Hello"))
  }

  it should "convert Array[_]" in {
    Mapping[Array[A], Array[B]].map(Array(A("Hello"))) shouldBe Array(B("Hello"))
  }

  it should "convert Map[String, _]" in {
    Mapping[Map[String, A], Map[String, B]].map(Map("1" -> A("Hello"))) shouldBe Map("1" -> B("Hello"))
  }

  it should "convert TreeMap[String, _]" in {
    Mapping[TreeMap[String, A], TreeMap[String, B]].map(TreeMap("1" -> A("Hello"))) shouldBe TreeMap("1" -> B("Hello"))
  }

  it should "convert Map with converting keys" in {
    implicit val keyMapping: Mapping[Int, String] = _.toString
    Mapping[Map[Int, A], Map[String, B]].map(Map(1 -> A("Hello"))) shouldBe Map("1" -> B("Hello"))
  }
}
