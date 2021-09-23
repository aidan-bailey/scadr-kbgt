package kbgt.logic

import kbgt._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import org.tweetyproject.logics.pl.parser.PlParser
import scala.io.Source
import java.io.PrintWriter
import java.io.File

/** Propositional parser. */
object Parser {

  private val parser = new PlParser()

  /** Tweety to classical formula conversion method.
    *
    * @param p
    *   PlFormula to be converted
    * @return
    *   classical formula counterpart of p
    */
  def tweety2formula(p: PlFormula): ClassicalFormula = {
    if (p.isInstanceOf[Implication]) {
      val formulas = p.asInstanceOf[Implication]
      return BinCon(
        BinOp.Implies,
        tweety2formula(formulas.getFirstFormula()),
        tweety2formula(formulas.getSecondFormula())
      )
    } else if (p.isInstanceOf[Conjunction]) {
      val formulas = p.asInstanceOf[Conjunction].getFormulas()
      return BinCon(
        BinOp.And,
        tweety2formula(formulas.get(0)),
        tweety2formula(formulas.get(1))
      )
    } else if (p.isInstanceOf[Disjunction]) {
      val formulas = p.asInstanceOf[Disjunction].getFormulas()
      return BinCon(
        BinOp.Or,
        tweety2formula(formulas.get(0)),
        tweety2formula(formulas.get(1))
      )
    } else if (p.isInstanceOf[ExclusiveDisjunction]) {
      val formulas = p.asInstanceOf[ExclusiveDisjunction].getFormulas()
      val left = formulas.get(0)
      val right = formulas.get(1)
      return BinCon(
        BinOp.Or,
        BinCon(
          BinOp.And,
          tweety2formula(left),
          UnCon(UnOp.Not, tweety2formula(right))
        ),
        BinCon(
          BinOp.And,
          tweety2formula(right),
          UnCon(UnOp.Not, tweety2formula(left))
        )
      )
    } else if (p.isInstanceOf[Equivalence]) {
      val formulas = p.asInstanceOf[Equivalence].getFormulas()
      return BinCon(
        BinOp.Iff,
        tweety2formula(formulas.getFirst()),
        tweety2formula(formulas.getSecond())
      )
    } else if (p.isInstanceOf[Tautology]) {
      return Const(
        Constant.Tautology
      )
    } else if (p.isInstanceOf[Contradiction]) {
      return Const(
        Constant.Contradiction
      )
    } else if (p.isInstanceOf[Negation]) {
      return UnCon(
        UnOp.Not,
        tweety2formula(p.asInstanceOf[Negation].getFormula())
      )
    }
    return Atom(p.asInstanceOf[Proposition].getName())
  }

  /** Parses a PlFormula.
    * @param string
    *   string to be parsed
    * @return
    *   PlFormula parsed from string
    */
  def parsePlFormula(string: String): PlFormula =
    parser.parseFormula(string).asInstanceOf[PlFormula]

  /** Parses a classical formula.
    * @param string
    *   string to be parsed
    * @return
    *   classical formula parsed from string
    */
  def parseClassicalFormula(string: String): ClassicalFormula =
    tweety2formula(parsePlFormula(string))

  /** Parses a defeasible formula.
    * @param string
    *   string to be parsed
    * @return
    *   defeasible formula parsed from string
    */
  def parseDefeasibleFormula(string: String): DefeasibleFormula = {
    val pos = string.indexOf("~>")
    val ante = parser.parseFormula(string.substring(0, pos))
    val desc = parser.parseFormula(string.substring(pos + 2))
    return DefeasibleFormula(tweety2formula(ante), tweety2formula(desc))
  }

  /** Parses a propositional formula.
    * @param string
    *   string to be parsed
    * @return
    *   formula parsed from string
    */
  def parseFormula(string: String): Formula = {
    if (string.contains("~>"))
      parseDefeasibleFormula(string)
    else
      parseClassicalFormula(string)
  }

  /** Parses a string intro a mixed knowledge base.
    * @param string
    *   string to be parsed
    * @return
    *   mixed knowledge base parsed from string
    */
  def parseString(
      string: String
  ): MixedKnowledgeBase = {
    var ckb = new ClassicalKnowledgeBase()
    var dkb = new DefeasibleKnowledgeBase()
    for (formulaString <- string.split(",")) {
      if (formulaString.contains("~>")) {
        val pos = formulaString.indexOf("~>")
        val ante = parser.parseFormula(formulaString.substring(0, pos))
        val desc = parser.parseFormula(formulaString.substring(pos + 2))
        dkb += DefeasibleFormula(tweety2formula(ante), tweety2formula(desc))
      } else {
        ckb += tweety2formula(parser.parseFormula(formulaString))
      }
    }
    return new MixedKnowledgeBase(ckb, dkb)
  }

}
