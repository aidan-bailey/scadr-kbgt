package skbgen.logic

import skbgen._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable

case class DefeasibleImplication(p: Formula, q: Formula) {
  override def toString(): String =
    p.toString() + "~>" + q.toString()
  def toParseString(): String =
    p.tweety.toString() + "~>" + q.tweety.toString()
  def materialize(): Formula = BinCon(BinOp.Implies, p, q)
  def antecedent: Formula = p
  def descendent: Formula = q
  def atoms: Set[Atom] =
    p.atoms() ++ q.atoms()

}
