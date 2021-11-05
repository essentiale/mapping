package essentiale.mapping.auto

/**
 * @author Konstantin Volchenko
 */
private[mapping] sealed trait MappingRule[S, T]

private[mapping] case class MappingRuleAssignment[S, T, V](source: S => V) extends MappingRule[S, T]
