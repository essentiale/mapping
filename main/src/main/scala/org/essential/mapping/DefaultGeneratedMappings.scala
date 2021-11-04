package org.essential.mapping

import org.essential.mapping.auto.AutoMappingMacro

import scala.language.experimental.macros
import scala.collection.{BuildFrom, IterableOnce}

/**
 * @author Konstantin Volchenko
 */
trait LowLevelGeneratedMappings {
  implicit def generatedRecursiveMapping[S, T]: GeneratedRecursiveMapping[S, T] = macro AutoMappingMacro.mapInlineRecursive[S, T]
}

trait DefaultGeneratedMappings extends LowLevelGeneratedMappings {

  implicit def identMapping[T]: GeneratedMapping[T, T] = source => source

  implicit def optionMapping[S, T](implicit m: GeneratedMapping[S, T]): GeneratedMapping[Option[S], Option[T]] =
    source => source.map(m.map)

  implicit def seqMapping[S, T, M[X] <: IterableOnce[X]](
    implicit m: GeneratedMapping[S, T],
    b: BuildFrom[M[S], T, M[T]]
  ): GeneratedMapping[M[S], M[T]] =
    source => b.fromSpecific(source)(source.iterator.map(m.map))

  implicit def optionMapping[A1, B1, A2, B2](
    implicit m1: GeneratedMapping[A1, A2],
    m2: GeneratedMapping[B1, B2]
  ): GeneratedMapping[Either[A1, B1], Either[A2, B2]] = {
    case Left(value)  => Left(m1.map(value))
    case Right(value) => Right(m2.map(value))
  }

  implicit def tuple1Mapping[S1, T1](implicit m1: GeneratedMapping[S1, T1]): GeneratedMapping[Tuple1[S1], Tuple1[T1]] = source =>
    Tuple1(m1.map(source._1))

  implicit def tuple2Mapping[S1, S2, T1, T2](
    implicit m1: GeneratedMapping[S1, T1],
    m2: GeneratedMapping[S2, T2]
  ): GeneratedMapping[Tuple2[S1, S2], Tuple2[T1, T2]] =
    source => Tuple2(m1.map(source._1), m2.map(source._2))

}
