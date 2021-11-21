package essentiale.mapping

import scala.collection.BuildFrom
import scala.collection.generic.{IsIterableOnce, IsSeq}

/**
 * Default implicits for Mapping trait for Map collections and other container types.
 *
 * @author Konstantin Volchenko
 */
class AuxIterableOnce[M, E](val value: IsIterableOnce[M] { type A = E })

object AuxIterableOnce {

  implicit def auxIterableOnce[M](implicit aux: IsIterableOnce[M]): AuxIterableOnce[M, aux.A] = new AuxIterableOnce[M, aux.A](aux)

}

trait MappingImplicits {

  protected def optionMappingImplicit[S, T](implicit m: Mapping[S, T]): Mapping[Option[S], Option[T]] =
    source => source.map(m.map)

  protected def seqMappingImplicit[S, T, C1, C2](
    implicit seqS: AuxIterableOnce[C1, S],
    seqT: AuxIterableOnce[C2, T],
    m: Mapping[S, T],
    b: BuildFrom[C1, T, C2]
  ): Mapping[C1, C2] = source => b.fromSpecific(source)(seqS.value(source).iterator.map(m.map))

  protected def eitherMappingImplicit[A1, B1, A2, B2](
    implicit m1: Mapping[A1, A2],
    m2: Mapping[B1, B2]
  ): Mapping[Either[A1, B1], Either[A2, B2]] = {
    case Left(value)  => Left(m1.map(value))
    case Right(value) => Right(m2.map(value))
  }

  protected def tuple1MappingImplicit[S1, T1](implicit m1: Mapping[S1, T1]): Mapping[Tuple1[S1], Tuple1[T1]] = source =>
    Tuple1(m1.map(source._1))

  protected def tuple2MappingImplicit[S1, S2, T1, T2](
    implicit m1: Mapping[S1, T1],
    m2: Mapping[S2, T2]
  ): Mapping[Tuple2[S1, S2], Tuple2[T1, T2]] =
    source => Tuple2(m1.map(source._1), m2.map(source._2))

}

trait DefaultMappingImplicits extends MappingImplicits {

  implicit def identMapping[T]: Mapping[T, T] = source => source

  implicit def optionMapping[S, T](implicit m: Mapping[S, T]): Mapping[Option[S], Option[T]] = optionMappingImplicit

  implicit def seqMapping[S, T, C1, C2](
    implicit seqS: AuxIterableOnce[C1, S],
    seqT: AuxIterableOnce[C2, T],
    m: Mapping[S, T],
    b: BuildFrom[C1, T, C2]
  ): Mapping[C1, C2] = seqMappingImplicit

  implicit def eitherMapping[A1, B1, A2, B2](implicit m1: Mapping[A1, A2], m2: Mapping[B1, B2]): Mapping[Either[A1, B1], Either[A2, B2]] =
    eitherMappingImplicit

  implicit def tuple1Mapping[S1, T1](implicit m1: Mapping[S1, T1]): Mapping[Tuple1[S1], Tuple1[T1]] = tuple1MappingImplicit

  implicit def tuple2Mapping[S1, S2, T1, T2](implicit m1: Mapping[S1, T1], m2: Mapping[S2, T2]): Mapping[Tuple2[S1, S2], Tuple2[T1, T2]] =
    tuple2MappingImplicit

}
