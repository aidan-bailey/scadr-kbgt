package kbgt.logic

import org.tweetyproject.logics.pl.syntax.PlFormula
import org.tweetyproject.logics.pl.sat.Sat4jSolver
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import org.tweetyproject.logics.pl.sat.SatSolver

/** A propositional formula. */
abstract trait Formula {

  SatSolver.setDefaultSolver(new Sat4jSolver())
  protected val solver = new Sat4jSolver()
  protected val reasoner = new SatReasoner()

  /** Gets the atoms of the formula.
    *
    * @return
    *   set of atoms in the formula
    */
  def atoms(): Set[Atom]

  /** Checks the formula's satisfiability.
    *
    * @return
    *   true if valid, false if otherwise
    */
  def isSatisfiable(): Boolean

  /** Checks the formula's validity.
    *
    * @return
    *   true if valid, false if otherwise
    */
  def isValid(): Boolean

  /** Gets the formula's parse string.
    *
    * @return
    *   the formula's parse string.
    */
  def getParseString(): String

  /** Gets the formula's TweetyProject counterpart.
    *
    * @return
    *   PlFormula counterpart of the formula
    */
  def getPlFormula(): PlFormula

  /** toString override. */
  override def toString(): String

}
