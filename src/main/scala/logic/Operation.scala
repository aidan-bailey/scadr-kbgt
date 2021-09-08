package skbgen.logic

import skbgen._
import org.tweetyproject.logics.pl.syntax._

sealed trait Operation extends Enumeration

object Notation {
  var choice: NotationOption.Value = NotationOption.Tweety
}

/** Binary operator enums and maps. */
object BinOp extends Operation {

  /** Binary operator enums. */
  val And, Or, Implies, Iff = Value

  def getParseNotation(op: BinOp.Value): String =
    op match {
      case And     => "&&"
      case Or      => "||"
      case Implies => "=>"
      case Iff     => "<=>"
    }

  /** Maps to symbolic representation. */
  def getNotation(op: BinOp.Value): String =
    Notation.choice match {
      case NotationOption.Tweety =>
        op match {
          case And     => "&&"
          case Or      => "||"
          case Implies => "=>"
          case Iff     => "<=>"
        }
      case NotationOption.Formal =>
        op match {
          case And     => "∧"
          case Or      => "∨"
          case Implies => "→"
          case Iff     => "↔"
        }
      case NotationOption.Latex =>
        op match {
          case And     => "\\land "
          case Or      => "\\vee  "
          case Implies => "\\rightarrow "
          case Iff     => "\\leftrightarrow "
        }
    }

  def getTweety(
      op: BinOp.Value,
      leftOperand: Formula,
      rightOperand: Formula
  ): PlFormula =
    op match {
      case BinOp.And =>
        new Conjunction(leftOperand.tweety(), rightOperand.tweety())
      case BinOp.Or =>
        new Disjunction(leftOperand.tweety(), rightOperand.tweety())
      case BinOp.Implies =>
        new Implication(leftOperand.tweety(), rightOperand.tweety())
      case BinOp.Iff =>
        new Equivalence(leftOperand.tweety(), rightOperand.tweety())
    }

  /** Maps to function. */
  val functionMap: Map[BinOp.Value, (Boolean, Boolean) => Boolean] =
    Map(
      And -> ((p: Boolean, q: Boolean) => p && q),
      Or -> ((p: Boolean, q: Boolean) => p || q),
      Implies -> ((p: Boolean, q: Boolean) => p <= q),
      Iff -> ((p: Boolean, q: Boolean) => p == q)
    )
}

/** Unary operator enums and maps. */
object UnOp extends Operation {

  /** Unary operator enums. */
  val Not = Value

  def getParseNotation(op: UnOp.Value): String =
    op match {
      case Not => "!"
    }

  /** Maps to symbolic representation. */
  def getNotation(op: UnOp.Value): String =
    Notation.choice match {
      case NotationOption.Tweety =>
        op match {
          case Not => "!"
        }
      case NotationOption.Formal =>
        op match {
          case Not => "¬"
        }
      case NotationOption.Latex =>
        op match {
          case Not => "\\neg "
        }
    }

  def getTweety(operator: UnOp.Value, operand: Formula): PlFormula =
    operator match {
      case UnOp.Not => new Negation(operand.tweety())
    }

  /** Maps to function. */
  val functionMap: Map[UnOp.Value, Boolean => Boolean] = Map(
    Not -> ((p: Boolean) => !p)
  )

}

object Constant extends Enumeration {
  val Tautology, Contradiction = Value

  def getParseNotation(op: Constant.Value): String =
    op match {
      case Tautology     => "+"
      case Contradiction => "-"
    }

  /** Maps to symbolic representation. */
  def getNotation(op: Constant.Value): String =
    Notation.choice match {
      case NotationOption.Tweety =>
        op match {
          case Tautology     => "+"
          case Contradiction => "-"
        }
      case NotationOption.Formal =>
        op match {
          case Tautology     => "⊤"
          case Contradiction => "⊥"
        }
      case NotationOption.Latex =>
        op match {
          case Tautology     => "\\top "
          case Contradiction => "\\bot "
        }
    }

  def getTweety(symbol: Constant.Value): PlFormula =
    symbol match {
      case Tautology     => new Tautology
      case Contradiction => new Contradiction
    }

  /** Maps to function. */
  val functionMap: Map[Constant.Value, Boolean] = Map(
    Tautology -> true,
    Contradiction -> false
  )
}
