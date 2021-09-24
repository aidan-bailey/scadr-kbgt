package kbgt.logic

import scala.collection.mutable
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import scala.jdk.CollectionConverters._
import java.io.PrintWriter
import java.io.File
import scala.io.Source
import org.tweetyproject.logics.pl.syntax.PlBeliefSet

/** A propositional knowledge base.
 *
 * @constructor create a knowledge base with a set of propositional formulas
 * @param kb the set of propositional formulas.
 */
abstract class KnowledgeBase[FormulaType <: Formula](
    kb: mutable.Set[FormulaType]
) extends mutable.Set[FormulaType] {

  protected val reasoner = new SatReasoner()

  /** Gets the atoms of the knowledge base.
    *
    * @return
    *   set of atoms in the knowledge base
    */
  def atoms(): Set[Atom] =
    this.flatMap(f => f.atoms()).toSet

  /** Gets the parse string of the knowledge base.
   *
   * @return the knowledge base's parse string
   */
  def toParseString(): String =
    kb.map(formula => formula.getParseString()).mkString(",")

  /** Writes the knowledge base to a file.
   *
   * @param filename the name of the file to output to.
   */
  def writeFile(filename: String): Unit = {
    val pw = new PrintWriter(new File(filename))
    if (isEmpty)
      pw.write("[]")
    else
      pw.write('['+toParseString().split(",").map(f => s"\"$f\"").mkString(",")+']')
    pw.close()
  }

  /** Writes the knowledge base to a file in scadr format.
   *
   * @param filename the name of the file to output to.
   */
  def writeScadrFile(filename: String): Unit = {
    val pw = new PrintWriter(new File(filename))
    pw.write(toParseString().split(",").mkString("\n"))
    pw.close()
  }

  /** Loads statements from specified file into knowledge base.
   *
   * @param filename the name of the file
   */
  def loadFile(filename: String): this.type

  /**  Gets the TweetyProject counterpart for the knowledge base.
   *
   * @return the PlBeliefSet counterpart of the knowledge base
   */
  def toPlBeliefSet(): PlBeliefSet = new PlBeliefSet(
      map(f => f.getPlFormula()).asJava
    )

  /** Adds a formula to the knowledge base.
   *
   * @param formula the formula to be added to the knowledge base
   */
  override def addOne(formula: FormulaType): this.type = {
    kb.addOne(formula)
    this
  }

  /** Subtracts a formula from the knowledge base.
   *
   * @param formula the formula to be subtracted from the knowledge base
   */
  override def subtractOne(formula: FormulaType): this.type = {
    kb.subtractOne(formula)
    this
  }

  /** Clears the knowledge base. */
  override def clear(): Unit = kb.clear()

  /** Gets an iterator for the knowledge base
   *
   * @return the formula iterator
   */
  override def iterator: Iterator[FormulaType] = kb.toIterator

  /** Checks if a formula is contained in the knowledge base.
   *
   * @param formula the formula of which to check
   * @return true if the formula is contained, false if otherwise
   */
  override def contains(formula: FormulaType): Boolean = kb.contains(formula)

  /** toSting override */
  override def toString(): String = {
    kb.map(formula => formula.toString()).toList.mkString(",")
  }

}
