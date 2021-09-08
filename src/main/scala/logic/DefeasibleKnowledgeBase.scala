package skbgen.logic

import skbgen.config._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable

class DefeasibleKnowledgeBase(
    kb: mutable.Set[DefeasibleImplication]
) extends collection.mutable.Set[DefeasibleImplication] {

  def this(formula: DefeasibleImplication*) = {
    this(mutable.Set(formula: _*))
  }

  def materialize(): ClassicalKnowledgeBase =
    new ClassicalKnowledgeBase(for (f <- kb) yield f.materialize())

  def writeFile(filename: String) = {
    val pw = new PrintWriter(new File(filename + ".kb"))
    pw.write(toParseString)
    pw.close()
  }

  def toParseString(): String = {
    var buffer = ""
    for (formula <- kb)
      buffer = buffer + formula.toParseString + ','
    if (buffer.nonEmpty)
      buffer = buffer.substring(0, buffer.size - 1)
    return buffer
  }

  // Set Overrides
  override def +(formula: DefeasibleImplication): DefeasibleKnowledgeBase =
    clone().addOne(formula)
  def ++(dkb: DefeasibleKnowledgeBase): DefeasibleKnowledgeBase =
    clone().addAll(dkb)
  override def -(formula: DefeasibleImplication): DefeasibleKnowledgeBase =
    clone().subtractOne(formula)
  def --(dkb: DefeasibleKnowledgeBase): DefeasibleKnowledgeBase =
    clone().subtractAll(dkb)
  override def iterator: Iterator[DefeasibleImplication] = kb.iterator
  override def contains(defImpl: DefeasibleImplication): Boolean =
    kb.contains(defImpl)
  override def addOne(
      elem: DefeasibleImplication
  ): this.type = {
    if (!kb.contains(elem)) this.kb += elem
    this
  }
  override def subtractOne(
      elem: DefeasibleImplication
  ): this.type = {
    if (kb.contains(elem)) this.kb -= elem
    this
  }
  override def clear(): Unit = kb.clear()
  override def clone(): DefeasibleKnowledgeBase =
    new DefeasibleKnowledgeBase(kb.clone())
  override def toString(): String = {
    kb.map(formula => formula.toString()).toList.mkString(",")
  }
}
