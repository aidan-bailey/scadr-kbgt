package kbgt.logic

import kbgt._
import org.tweetyproject.logics.pl.syntax._

/** A connective operator.
  *
  * Extends [[scala.Enumeration]]
  */
sealed trait Operator extends Enumeration {

  /** Gets operator notation.
    *
    * @param op
    *   the operator value
    * @param notationOption
    *   the notation value
    * @return
    *   the operator notation string
    */
  def getNotation(op: Value, notationOption: Notation.Value): String

  /** Get the operators parse notation.
    *
    * @param op
    *   the operator value
    * @return
    *   the operator parse notation string
    */
  def getParseNotation(op: Value): String
}

/** Operator notations.
  *
  * Extends [[scala.Enumeration]]
  */
object Notation extends Enumeration {
  val Tweety, Latex, Formal = Value
}

/** The Binary connective operators.
  *
  * Extends [skbgen.logic.Operator]
  */
object BinOp extends Operator {

  val And, Or, Implies, Iff = Value

  /** Gets operator notation.
    *
    * @param op
    *   the operator value
    * @param notationOption
    *   the notation value
    * @return
    *   the operator notation string
    */
  override def getNotation(
      operator: BinOp.Value,
      notationOption: Notation.Value
  ): String =
    notationOption match {
      case Notation.Tweety =>
        operator match {
          case And     => "&&"
          case Or      => "||"
          case Implies => "=>"
          case Iff     => "<=>"
        }
      case Notation.Formal =>
        operator match {
          case And     => "∧"
          case Or      => "∨"
          case Implies => "→"
          case Iff     => "↔"
        }
      case Notation.Latex =>
        operator match {
          case And     => "\\land "
          case Or      => "\\vee  "
          case Implies => "\\rightarrow "
          case Iff     => "\\leftrightarrow "
        }
    }

  /** Get the operators parse notation.
    *
    * @param op
    *   the operator value
    * @return
    *   the operator parse notation string
    */
  override def getParseNotation(op: BinOp.Value): String =
    getNotation(op, Notation.Tweety)

  /** Get TweetyProject's PlFormula counterpart.
    *
    * @param op
    *   the operator vale
    * @param leftOperand
    *   the left classical formula operand
    * @param rightOperand
    *   the right classical formula operand
    * @return
    *   the PlFormula counterpart for the binary connective
    */
  def getPlFormula(
      op: BinOp.Value,
      leftOperand: ClassicalFormula,
      rightOperand: ClassicalFormula
  ): PlFormula =
    op match {
      case BinOp.And =>
        new Conjunction(leftOperand.getPlFormula(), rightOperand.getPlFormula())
      case BinOp.Or =>
        new Disjunction(leftOperand.getPlFormula(), rightOperand.getPlFormula())
      case BinOp.Implies =>
        new Implication(leftOperand.getPlFormula(), rightOperand.getPlFormula())
      case BinOp.Iff =>
        new Equivalence(leftOperand.getPlFormula(), rightOperand.getPlFormula())
    }

}

/** The Unary connective operators.
  *
  * Extends [[skbgen.logic.Operator]]
  */
object UnOp extends Operator {

  val Not = Value

  /** Gets operator notation.
    *
    * @param op
    *   the operator value
    * @param notationOption
    *   the notation value
    * @return
    *   the operator notation string
    */
  override def getNotation(
      op: UnOp.Value,
      notationOption: Notation.Value
  ): String =
    notationOption match {
      case Notation.Tweety =>
        op match {
          case Not => "!"
        }
      case Notation.Formal =>
        op match {
          case Not => "¬"
        }
      case Notation.Latex =>
        op match {
          case Not => "\\neg "
        }
    }

  /** Get the operators parse notation.
    *
    * @param op
    *   the operator value
    * @return
    *   the operator parse notation string
    */
  override def getParseNotation(op: UnOp.Value): String =
    getNotation(op, Notation.Tweety)

  /** Get TweetyProject's PlFormula counterpart.
    *
    * @param op
    *   the operator vale
    * @param operand
    *   the classical formula operand
    * @return
    *   the PlFormula counterpart for the unary connective
    */
  def getPlFormula(operator: UnOp.Value, operand: ClassicalFormula): PlFormula =
    operator match {
      case UnOp.Not => new Negation(operand.getPlFormula())
    }

}

/** The constant symbol values.
  *
  * Extends [[scala.Enumeration]]
  */
object Constant extends Enumeration {

  val Tautology, Contradiction = Value

  /** Gets constant notation.
    *
    * @param op
    *   the constant symbol
    * @param notationOption
    *   the notation value
    * @return
    *   the constant notation string
    */
  def getNotation(
      op: Constant.Value,
      notationOption: Notation.Value
  ): String =
    notationOption match {
      case Notation.Tweety =>
        op match {
          case Tautology     => "+"
          case Contradiction => "-"
        }
      case Notation.Formal =>
        op match {
          case Tautology     => "⊤"
          case Contradiction => "⊥"
        }
      case Notation.Latex =>
        op match {
          case Tautology     => "\\top "
          case Contradiction => "\\bot "
        }
    }

  /** Get the Constants parse notation.
    *
    * @param op
    *   the constant symbol
    * @return
    *   the constant parse notation string
    */
  def getParseNotation(op: Constant.Value): String =
    getNotation(op, Notation.Tweety)

  /** Get TweetyProject's PlFormula counterpart.
    *
    * @param symbol
    *   the constant symbol
    * @return
    *   the PlFormula counterpart for the constant
    */
  def getPlFormula(symbol: Constant.Value): PlFormula =
    symbol match {
      case Tautology     => new Tautology
      case Contradiction => new Contradiction
    }

}
