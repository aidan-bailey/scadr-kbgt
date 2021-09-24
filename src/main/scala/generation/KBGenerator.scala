package kbgt.generation

import kbgt.logic._
import kbgt._
import scala.collection._
import java.io._
import scala.util.Random

/** A collection of knowledge base generation methods. */
object KBGenerator {

  /** Generates a knowledge base.
    *
    * @param r
    *   number of defeasible ranks
    * @param s
    *   number of defeasible statements
    * @param distributionFunction
    *   distribution of defeasible statements
    * @param defeasibleOnly
    *   whether or not the knowledge base be defeasible only
    * @return
    *   a mixed knowledge base
    */
  def RankedGenerate(
      r: Int,
      s: Int,
      distributionFunction: (Int, Int, Int) => Int,
      defeasibleOnly: Boolean
  ) = {
    var A = mutable.Set[Atom]()
    var C = mutable.Set[ConflictRelation]()
    var D = mutable.Set[DefeasibleRelation]()
    var j = 0
    val lowerBound = if (defeasibleOnly) 3 else 2
    while (j < r) {
      val antecedent = Atom(j.toString())
      A += antecedent
      if (j > 0) {
        val consequent = Atom((j - 1).toString())
        C += (ConflictRelation(antecedent, consequent))
      }
      var i =
        if (defeasibleOnly) {
          if (r == 1)
            0
          else if (j == 0)
            1
          else if (j == r - 1)
            2
          else
            3
        } else {
          if (r == 1 || j == 0 || j == r - 1)
            1
          else
            2
        }
      while (i < math.max(distributionFunction(s, r, j + 1), lowerBound)) {
        val defantecedent = Atom(s"$j.$i")
        A += defantecedent
        D += DefeasibleRelation(defantecedent, antecedent)
        i += 1
      }
      j += 1
    }
    val ddg = new DCG(A, C, D)
    ddg.DDGKB(defeasibleOnly)
  }

  /** Generates a knowledge base using the conservative method.
    *
    * @param r
    *   number of defeasible ranks
    * @param s
    *   number of defeasible statements
    * @param distributionFunction
    *   distribution of defeasible statements
    * @param defeasibleOnly
    *   whether or not the knowledge base be defeasible only
    * @return
    *   a mixed knowledge base
    */
  def ConservativeRankedGenerate(
      r: Int,
      s: Int,
      distributionFunction: (Int, Int, Int) => Int,
      defeasibleOnly: Boolean
  ) = {
    var A = mutable.Set[Atom]()
    var C = mutable.Set[ConflictRelation]()
    var D = mutable.Set[DefeasibleRelation]()
    var j = 0
    var lowerBound =
      if (defeasibleOnly)
        2
      else
        1
    while (j < r) {
      val antecedent = Atom(j.toString())
      A += antecedent
      if (j > 0)
        C += ConflictRelation(antecedent, Atom((j - 1).toString()))
      var i =
        if (defeasibleOnly) {
          if (r == 1)
            0
          if (j == 0)
            1
          else
            2
        } else {
          if (r == 1)
            0
          else
            1
        }
      while (i < math.max(distributionFunction(s, r, j + 1), lowerBound)) {
        val defantecedent = Atom(s"$j.$i")
        A += defantecedent
        D += DefeasibleRelation(defantecedent, antecedent)
        i += 1
      }
      j += 1
    }
    val ddg = new DCG(A, C, D)
    ddg.ChainDCGKB(defeasibleOnly)
  }

  def GenerateRanked(
      r: Int,
      s: Int,
      d: (Int, Int, Int) => Int,
      defeasibleOnly: Boolean
  ): MixedKnowledgeBase = {
    val k = new MixedKnowledgeBase()
    var lowerBound = 1
    if (defeasibleOnly)
      lowerBound = lowerBound + 1
    val contradictionAtom = Atom("B")
    var j = 0
    while (j < r) {
      val anchorAtom = Atom(s"$j")
      if (j == 0) {
        k += DefeasibleFormula(anchorAtom, contradictionAtom)
        if (defeasibleOnly) {
          var paddingAtom = Atom("d")
          k += DefeasibleFormula(paddingAtom, anchorAtom)
        }
      } else {
        val prevAnchor = Atom((j - 1).toString())
        if (defeasibleOnly)
          k += DefeasibleFormula(anchorAtom, prevAnchor)
        else
          k += BinCon(BinOp.Implies, anchorAtom, prevAnchor)
        if ((j + 1) % 2 == 0)
          k += DefeasibleFormula(anchorAtom, UnCon(UnOp.Not, contradictionAtom))
        else
          k += DefeasibleFormula(anchorAtom, contradictionAtom)
      }
      var i = 0
      while (i < d(s, r, j + 1) - lowerBound) {
        val extensionAtom = Atom(s"$j.$i")
        k += DefeasibleFormula(extensionAtom, anchorAtom)
        i += 1
      }
      j += 1
    }
    return k
  }

}
