package essentiale.mapping

/**
 * @author Konstantin Volchenko
 */

/**
 * Mapping from <code>S</code> class to <code>T</code>. Class <code>T</code> should be a case class.
 */
trait Mapping[S, T] {

  /**
   * Map source class to target.
   *
   * @param source source class.
   * @return target class.
   */
  def map(source: S): T
}

object Mapping {

  /**
   * Summon implicit <code>Mapping[S, T]</code>
   */
  def apply[S, T](implicit m: Mapping[S, T]): Mapping[S, T] = m

}
