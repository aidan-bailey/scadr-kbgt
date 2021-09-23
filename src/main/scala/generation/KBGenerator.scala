package kbgt.generation

import kbgt.logic._
import kbgt._
import scala.collection._
import java.io._
import scala.util.Random

/** A collection of knowledge base generation methods. */
object KBGenerator {

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
        if (r == 1)
          0
        else if (j == r - 1 || j == 0)
          1
        else
          2
      while (
        i < math.max(distributionFunction(s, r, j + 1) - lowerBound, lowerBound)
      ) {
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
        if (r == 1 || j == 0)
          1
        else
          2
      while (
        i < math.max(distributionFunction(s, r, j + 1) - lowerBound, lowerBound)
      ) {
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

}
