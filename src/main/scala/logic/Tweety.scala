package skbgen.logic

import skbgen.config._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import org.tweetyproject.logics.pl.parser.PlParser
import scala.io.Source

object Tweety {

  private val parser = new PlParser()

  SatSolver.setDefaultSolver(new Sat4jSolver)
  val solver = new Sat4jSolver()
  val reasoner = new SatReasoner()

  /** Tweety to formula converter */
  implicit def tweety2formula(p: PlFormula): Formula = {
    if (p.isInstanceOf[Implication]) {
      val formulas = p.asInstanceOf[Implication]
      return BinCon(
        BinOp.Implies,
        formulas.getFirstFormula(),
        formulas.getSecondFormula()
      )
    } else if (p.isInstanceOf[Conjunction]) {
      val formulas = p.asInstanceOf[Conjunction].getFormulas()
      return BinCon(
        BinOp.And,
        formulas.get(0),
        formulas.get(1)
      )
    } else if (p.isInstanceOf[Disjunction]) {
      val formulas = p.asInstanceOf[Disjunction].getFormulas()
      return BinCon(
        BinOp.Or,
        formulas.get(0),
        formulas.get(1)
      )
    } else if (p.isInstanceOf[ExclusiveDisjunction]) {
      val formulas = p.asInstanceOf[ExclusiveDisjunction].getFormulas()
      val left = formulas.get(0)
      val right = formulas.get(1)
      return BinCon(
        BinOp.Or,
        BinCon(BinOp.And, left, UnCon(UnOp.Not, right)),
        BinCon(BinOp.And, right, UnCon(UnOp.Not, left))
      )
    } else if (p.isInstanceOf[Equivalence]) {
      val formulas = p.asInstanceOf[Equivalence].getFormulas()
      return BinCon(
        BinOp.Iff,
        formulas.getFirst(),
        formulas.getSecond()
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
      return UnCon(UnOp.Not, p.asInstanceOf[Negation].getFormula())
    }
    return Atom(p.asInstanceOf[Proposition].getName())
  }

  def tweetify(string: String): String = {
    var replace =
      Map(
        "&&" -> List("∧", "AND", "and"),
        "||" -> List("∨", "OR", "or"),
        "=>" -> List("→", "->", "IMPLIES", "implies"),
        "<=>" -> List("↔", "EQUALS", "equals"),
        "+" -> List("⊤", "True", "false"),
        "-" -> List("⊥", "False", "false")
      )
    var result = string
    for (key <- replace.keySet)
      for (operation <- replace(key))
        result = result.replaceAllLiterally(operation, key)
    return result
  }

  def parseTweety(string: String): PlFormula =
    parser.parseFormula(tweetify(string)).asInstanceOf[PlFormula]

  def parseFormula(string: String): Formula =
    tweety2formula(parseTweety(string))

  def parseCKB(string: String): ClassicalKnowledgeBase = {
    var kb = new ClassicalKnowledgeBase()
    for (formula <- string.split(",").map(parser.parseFormula))
      kb += tweety2formula(formula)
    return kb
  }

  def parseFile(
      filename: String
  ): MixedKnowledgeBase = {
    var ckb = new ClassicalKnowledgeBase()
    var dkb = new DefeasibleKnowledgeBase()
    for (line <- Source.fromFile(filename + ".kb").getLines()) {
      for (formulaString <- line.split(",")) {
        if (formulaString.contains("~>")) {
          val pos = formulaString.indexOf("~>")
          val ante = parser.parseFormula(formulaString.substring(0, pos))
          val desc = parser.parseFormula(formulaString.substring(pos + 2))
          dkb += DefeasibleImplication(ante, desc)
        } else {
          ckb += parser.parseFormula(formulaString)
        }
      }
    }
    return new MixedKnowledgeBase(ckb, dkb)
  }

  def parseRankedKB(filename: String): RankedKnowledgeBase = {
    val parser = new PlParser()
    var result = ListBuffer[ClassicalKnowledgeBase]()
    for (line <- Source.fromFile(filename + ".ranked").getLines())
      result += parseCKB(line)
    return new RankedKnowledgeBase(result.toList)
  }

}
