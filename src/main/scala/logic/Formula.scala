package skbgen.logic

import skbgen._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable
import skbgen.logic.Tweety

/** Defines a propositional formula. */
sealed abstract trait Formula {

  // Abstract Methods

  /** Evaluates a propositional formula. */
  def evaluate(valuation: Map[String, Boolean]): Boolean

  /** Returns the set of atoms. */
  def atoms(): Set[Atom]

  /** Returns the formula's negation. */
  def negate(): Formula

  /** Returns the formula's tweety formula. */
  def tweety(): PlFormula

  /** Returns the formula's parse string. */
  def toParseString(): String

  /** toString override. */
  override def toString(): String

  // Concrete Methods

  /** Returns the set of possible valuations. */
  def world(): Set[Map[String, Boolean]] = {
    var atomSeq = atoms().toSeq
    var atomCount = atomSeq.size
    var valuations: Set[Map[String, Boolean]] = Set()
    var bound = math.pow(2, atomCount).toInt
    for (i <- 0 to bound - 1) {
      var valuation: Map[String, Boolean] = Map()
      var binString = i.toBinaryString
      binString = "0" * (atomCount - binString.length()) + binString
      for (j <- 0 to atomCount - 1) {
        var assignment = binString(j) == '1'
        var atom = atomSeq(j)
        valuation += (atom.toString -> assignment)
      }
      valuations += valuation
    }
    return valuations
  }

  /** Returns the set of models. */
  def models(): Set[Map[String, Boolean]] = world().filter(v => evaluate(v))

  /** Checks entailment. */
  def entails(formula: Formula): Boolean =
    Tweety.reasoner.query(tweety(), formula.tweety())

  /** Checks validity. */
  def isValid(): Boolean = Tweety.reasoner.query(new PlBeliefSet(), tweety())

  /** Checks satisfiability. */
  def isSatisfiable(): Boolean = Tweety.solver.isSatisfiable(new PlBeliefSet() {
    tweety()
  })

}

/** Represents a propositional atom. */
case class Atom(name: String) extends Formula {

  /** Evaluates a propositional formula. */
  override def evaluate(valuation: Map[String, Boolean]): Boolean = valuation(
    name
  )

  override def toParseString(): String = name

  /** Returns the set of atoms. */
  override def atoms() = Set(this)

  /** Returns the formula's negation. */
  override def negate(): Formula = UnCon(UnOp.Not, this)

  /** Returns the formula's tweety formula. */
  override def tweety(): PlFormula = new Proposition(name)

  /** toString override. */
  override def toString(): String = name

}

/** Represents a propositional constant. */
case class Const(symbol: Constant.Value) extends Formula {

  /** Evaluates a propositional constant. */
  override def evaluate(valuation: Map[String, Boolean]): Boolean =
    Constant.functionMap(symbol)

  /** Returns the set of atoms. */
  override def atoms() = Set()

  /** Returns the formula's negation. */
  override def negate(): Formula = UnCon(UnOp.Not, this)

  /** Returns the formula's tweety formula. */
  override def tweety(): PlFormula = Constant.getTweety(symbol)

  /** toString override. */
  override def toString(): String = Constant.getNotation(symbol)

  override def toParseString(): String = Constant.getParseNotation(symbol)

}

/** Represents a propositional binary connective. */
case class BinCon(
    operator: BinOp.Value,
    leftOperand: Formula,
    rightOperand: Formula
) extends Formula {

  /** Evaluates a propositional binary connective. */
  override def evaluate(valuation: Map[String, Boolean]): Boolean = {
    return BinOp.functionMap(operator)(
      leftOperand.evaluate(valuation),
      rightOperand.evaluate(valuation)
    )
  }

  /** Returns the set of atoms. */
  override def atoms() =
    leftOperand.atoms() ++ rightOperand.atoms()

  /** Returns the formula's negation. */
  override def negate(): Formula = UnCon(UnOp.Not, this)

  /** Returns the formula's tweety formula. */
  override def tweety(): PlFormula =
    BinOp.getTweety(operator, leftOperand, rightOperand)

  /** toString override. */
  override def toString(): String = {
    return "(" + leftOperand.toString() + BinOp.getNotation(
      operator
    ) + rightOperand.toString() + ")"
  }

  override def toParseString(): String = {
    return "(" + leftOperand.toString() + BinOp.getParseNotation(
      operator
    ) + rightOperand.toString() + ")"
  }

}

/** Represents a propositional unary connective. */
case class UnCon(operator: UnOp.Value, operand: Formula) extends Formula {

  /** Evaluates a propositional unary connective. */
  override def evaluate(valuation: Map[String, Boolean]): Boolean = {
    return UnOp.functionMap(operator)(operand.evaluate(valuation));
  }

  /** Returns the set of atoms. */
  override def atoms() =
    operand.atoms()

  /** Returns the formula's tweety formula. */
  override def tweety(): PlFormula = UnOp.getTweety(operator, operand)

  /** toString override. */
  override def toString(): String = {
    return "(" + UnOp.getNotation(operator) + operand
      .toString() + ")";
  }

  override def toParseString(): String = {
    return "(" + UnOp.getParseNotation(
      operator
    ) + operand.toString() + ")"
  }

  /** Returns the formula's negation. */
  override def negate(): Formula = {
    return operand;
  }

}
