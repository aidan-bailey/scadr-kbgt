package kbgt.logic

import kbgt._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable
import kbgt.logic.Parser
import org.tweetyproject.logics.pl.semantics.PossibleWorld

/** A classical propositional formula.
  *
  * Extends [[skbgen.formula]]
  */
sealed abstract trait ClassicalFormula extends Formula {

  /** Checks whether this classical formula entails another classical formula.
    *
    * @param formula
    *   the other classical formula
    * @return
    *   true if this formula entails the other formula, false if otherwise
    */
  def entails(formula: ClassicalFormula) =
    reasoner.query(getPlFormula(), formula.getPlFormula())

  /** Checks the classical formula's satisfiability.
    *
    * @return
    *   true if satisfiable, false if otherwise
    */
  override def isSatisfiable() = solver.isSatisfiable(new PlBeliefSet() {
    getPlFormula()
  })

  /** Checks the classical formula's validity.
    *
    * @return
    *   true if valid, false if otherwise
    */
  override def isValid() = reasoner.query(new PlBeliefSet(), getPlFormula())

}

/** A propositional atom.
  *
  * Extends [[skbgen.logic.ClassicalFormula]]
  * @constructor
  *   create a new atom with a name
  * @param name
  *   the atom name
  */
case class Atom(name: String) extends ClassicalFormula {

  /** Gets the atoms in the classical formula.
    *
    * @return
    *   set of atoms in the classical formula
    */
  override def atoms() = Set(this)

  /** Gets the classical formula's parse string.
    *
    * @return
    *   the classical formula's parse string.
    */
  override def getParseString() = name

  /** Gets the classical formula's TweetyProject counterpart.
    *
    * @return
    *   PlFormula version of formula
    */
  override def getPlFormula() = new Proposition(name)

  /** toString override. */
  override def toString() = name

}

/** A propositional constant.
  *
  * Extends [[skbgen.logic.ClassicalFormula]]
  * @constructor
  *   create a new constant with a constant symbol
  * @param symbol
  *   the constant symbol
  */
case class Const(symbol: Constant.Value) extends ClassicalFormula {

  /** Gets atoms in the classical formula.
    *
    * @return
    *   set of atoms in the classical formula
    */
  override def atoms() = Set()

  /** Gets the classical formula's parse string.
    *
    * @return
    *   the classical formula's parse string.
    */
  override def getParseString() = Constant.getParseNotation(symbol)

  /** Gets the classical formula's TweetyProject counterpart.
    *
    * @return
    *   PlFormula version of classical formula
    */
  override def getPlFormula() = Constant.getPlFormula(symbol)

  /** toString override. */
  override def toString() = Constant.getNotation(symbol, Notation.Formal)

}

/** A Binary connective.
  *
  * Extends [[skbgen.logic.ClassicalFormula]]
  * @constructor
  *   create a new binary connective with a binary operator, left classical
  *   formula operand and right classical formula operand
  * @param operator
  *   the binary connective operator
  * @param leftOperand
  *   the left classical formula operand
  * @param rightOperand
  *   the right classical formula operand
  */
case class BinCon(
    operator: BinOp.Value,
    leftOperand: ClassicalFormula,
    rightOperand: ClassicalFormula
) extends ClassicalFormula {

  /** Gets the atoms in the classical formula.
    *
    * @return
    *   set of atoms in the classical formula
    */
  override def atoms() =
    leftOperand.atoms() ++ rightOperand.atoms()

  /** Gets the classical formula's parse string.
    *
    * @return
    *   the classical formula's parse string.
    */
  override def getParseString() =
    s"(${leftOperand + BinOp.getParseNotation(operator) + rightOperand})"

  /** Gets the classical formula's TweetyProject counterpart.
    *
    * @return
    *   PlFormula counterpart of the binary connective
    */
  override def getPlFormula() =
    BinOp.getPlFormula(operator, leftOperand, rightOperand)

  /** toString override. */
  override def toString() =
    s"(${leftOperand + BinOp.getNotation(operator, Notation.Formal) + rightOperand})"

}

/** A propositional unary connective.
  *
  * Extends [[skbgen.logic.ClassicalFormula]]
  * @constructor
  *   create a new unary connective with a unary operator and classical formula
  *   operand
  * @param operator
  *   the binary connective operator
  * @param operand
  *   the classical formula operand
  */
case class UnCon(operator: UnOp.Value, operand: ClassicalFormula)
    extends ClassicalFormula {

  /** Gets the atoms in the classical formula.
    *
    * @return
    *   set of atoms in the classical formula
    */
  override def atoms() = operand.atoms()

  /** Gets the classical formula's parse string.
    *
    * @return
    *   the classical formula's parse string.
    */
  override def getParseString() =
    s"(${UnOp.getParseNotation(operator) + operand})"

  /** Gets the classical formula's TweetyProject counterpart.
    *
    * @return
    *   PlFormula counterpart of the classical formula
    */
  override def getPlFormula() = UnOp.getPlFormula(operator, operand)

  /** toString override. */
  override def toString() =
    s"(${UnOp.getNotation(operator, Notation.Formal) + operand})"

}
