package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  /** Rather inefficient implementations:
    *  – same dependencies are potentially computed multiples times
    *  – eval helper (impl) is not tail recursive
    */

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.foldLeft(Map[String, Signal[Double]]()) { case (acc, (key, value)) =>
      acc + (key -> Signal { eval(value(), namedExpressions) })
    }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def impl(expr: Expr, references: Map[String, Signal[Expr]], dependencies: Set[String] = Set()): Double = expr match {
      case Literal(l) => l
      case Ref(r) => if (dependencies.contains(r) || !references.contains(r)) Double.NaN else impl(references(r)(), references, dependencies + r)

      case Divide(a, b) => impl(a, references, dependencies) / impl(b, references, dependencies)
      case Minus(a, b) => impl(a, references, dependencies) - impl(b, references, dependencies)
      case Plus(a, b) => impl(a, references, dependencies) + impl(b, references, dependencies)
      case Times(a, b) => impl(a, references, dependencies) * impl(b, references, dependencies)
    }

    impl(expr, references)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) =
    references.get(name).fold[Expr] { Literal(Double.NaN) } { exprSignal => exprSignal() }

}
