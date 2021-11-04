package org.essential.mapping

/**
 * @author Konstantin Volchenko
 */
trait Mapping[S, T] {
  def map(source: S): T
}

object Mapping {
  def apply[S, T](implicit m: Mapping[S, T]): Mapping[S, T] = m
}
