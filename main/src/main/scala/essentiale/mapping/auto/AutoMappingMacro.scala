package essentiale.mapping.auto

import scala.reflect.macros.blackbox

/**
 * @author Konstantin Volchenko
 */
object AutoMappingMacro {

  def buildMapping[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    new MappingGenerator(c).buildMapping[S, T].asInstanceOf[c.Tree]
  }

  def buildMappingRecursive[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    new MappingGenerator(c).buildMappingRecursive[S, T].asInstanceOf[c.Tree]
  }

  def mapInline[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    new MappingGenerator(c).mapInline[S, T].asInstanceOf[c.Tree]
  }

  def mapInlineRecursive[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    new MappingGenerator(c).mapInlineRecursive[S, T].asInstanceOf[c.Tree]
  }

  def buildMappingWithRules[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context)(rules: c.Expr[MappingRule[S, T]]*): c.Tree = {
    val generator = new MappingGenerator(c)
    generator.buildMappingWithRules[S, T](rules.map(_.asInstanceOf[generator.c.Expr[MappingRule[S, T]]]): _*).asInstanceOf[c.Tree]
  }

  def buildMappingRecursiveWithRules[S: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context)(rules: c.Expr[MappingRule[S, T]]*): c.Tree = {
    val generator = new MappingGenerator(c)
    generator.buildMappingRecursiveWithRules[S, T](rules.map(_.asInstanceOf[generator.c.Expr[MappingRule[S, T]]]): _*).asInstanceOf[c.Tree]
  }

}
