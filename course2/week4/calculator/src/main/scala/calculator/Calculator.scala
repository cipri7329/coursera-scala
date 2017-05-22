package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  //list of named variables
  /**
    * The function should return another map
    * from the same set of variable names
    * to their actual values,
    * computed from their expressions.
    * @param namedExpressions
    * @return
    */
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
//        namedExpressions.map{
//          case (varName, signalExpr) => {
//            varName -> Signal(eval(signalExpr(), namedExpressions))
//          }
//        }

      for {
        (variable, expression) <- namedExpressions
      } yield { variable -> Signal(eval(expression(), namedExpressions)) }
  }


  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    //detect cyclic dependencies
    case Ref(n) => {
      val filteredReferences = references - n
      val ref = getReferenceExpr(n, references)
//      eval(getReferenceExpr(n, references), references.filterKeys(_ != n))
      eval(ref, filteredReferences)
    }

    case Plus(a,b) => eval(a, references) +  eval(b, references)
    case Minus(a,b) => eval(a, references) -  eval(b, references)
    case Times(a,b) => eval(a, references) *  eval(b, references)
    case Divide(a,b) => eval(a, references) /  eval(b, references)
    case _ => Double.NaN
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
