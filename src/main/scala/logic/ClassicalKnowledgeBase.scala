package skbgen.logic

import skbgen._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable

class ClassicalKnowledgeBase(kb: mutable.Set[Formula])
    extends mutable.Set[Formula] {

  def this(formula: Formula*) =
    this(mutable.Set(formula: _*))

  def atoms(): Set[Atom] = {
    var result = new ListBuffer[Atom]()
    for (p <- kb)
      result ++= p.atoms()
    return result.toSet
  }

  /** Returns the set of models. */
  def models(): Set[Map[String, Boolean]] = {
    var iter = iterator
    if (iter.isEmpty)
      return Set()
    var formula = iter.next()
    while (iter.hasNext)
      formula = BinCon(BinOp.And, formula, iter.next())
    var modelSet = formula.models()
    return modelSet
  }

  /** Returns the PlBeliefSet of the models. */
  def tweety(): PlBeliefSet = {
    var iter = iterator
    if (iter.isEmpty)
      return new PlBeliefSet()
    var result = new PlBeliefSet()
    while (iter.hasNext)
      result.add(iter.next().tweety())
    return result
  }

  /** Checks entailment. */
  def entails(formula: Formula): Boolean = {
    Tweety.reasoner.query(tweety, formula.tweety())
  }

  def writeFile(filename: String) = {
    val pw = new PrintWriter(new File(filename + ".kb"))
    pw.write(toParseString)
    pw.close()
  }

  def toParseString(): String = {
    var buffer = ""
    for (formula <- kb)
      buffer = buffer + formula.tweety.toString() + ','
    if (buffer.nonEmpty)
      buffer = buffer.substring(0, buffer.size - 1)
    return buffer
  }

  def ++(ckb: ClassicalKnowledgeBase): ClassicalKnowledgeBase =
    clone().addAll(ckb)

  def --(ckb: ClassicalKnowledgeBase): ClassicalKnowledgeBase =
    clone().subtractAll(ckb)

  // Set Overrides
  override def clone(): ClassicalKnowledgeBase =
    new ClassicalKnowledgeBase(kb.clone())
  override def iterator: Iterator[Formula] = kb.iterator
  override def contains(formula: Formula): Boolean = kb.contains(formula)
  override def +(formula: Formula): ClassicalKnowledgeBase =
    clone().addOne(formula)
  override def -(formula: Formula): ClassicalKnowledgeBase =
    clone().subtractOne(formula)
  override def addOne(
      elem: Formula
  ): this.type = {
    if (!kb.contains(elem)) this.kb += elem
    this
  }
  override def subtractOne(
      elem: Formula
  ): this.type = {
    if (kb.contains(elem)) this.kb -= elem
    this
  }
  override def clear(): Unit = kb.clear()
  override def toString(): String = {
    kb.map(formula => formula.toString()).toList.mkString(",")
  }

}
