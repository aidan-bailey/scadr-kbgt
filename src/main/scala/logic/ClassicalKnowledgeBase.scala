package kbgt.logic

import kbgt._
import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable
import java.io.FileWriter
import scala.io.Source

/** A classical propositional knowledge base.
  *
  * Extends [[skbgen.logic.KnowledgeBase]]
  *
  * @constructor
  *   create a new classical knowledge base with a sequence of classical
  *   formulas
  * @param formulas
  *   the sequence of classical formulas.
  */
class ClassicalKnowledgeBase(formulas: ClassicalFormula*)
    extends KnowledgeBase[ClassicalFormula](
      mutable.Set[ClassicalFormula](formulas: _*)
    ) {

  /** Checks whether the classical knowledge base is satisfiable.
    *
    * @return
    *   true if satisfiable, false if otherwise
    */
  def isSatisfiable(): Boolean =
    forall(p => p.isSatisfiable())

  /** Checks whether a given classical formula is entailed by the classical
    * knowledge base.
    *
    * @param formula
    *   the classical formula to check entailment of
    * @return
    *   true if the classical formula is entailed, false if otherwise
    */
  def entails(formula: ClassicalFormula): Boolean =
    reasoner.query(toPlBeliefSet, formula.getPlFormula())

  /** Loads classical statements from a specified file into the classical
    * knowledge base.
    *
    * @param filename
    *   the filename of the specified file
    */
  override def loadFile(filename: String): this.type = {
    val strings = Source
      .fromFile(filename)
      .getLines()
      .next()
      .init
      .tail
    if (!strings.isEmpty)
      addAll(
        strings
          .split(",")
          .map(string => Parser.parseClassicalFormula(string.init.tail))
      )
    this
  }

  /** clone override. */
  override def clone(): ClassicalKnowledgeBase =
    new ClassicalKnowledgeBase(iterator.toSeq: _*)

}
