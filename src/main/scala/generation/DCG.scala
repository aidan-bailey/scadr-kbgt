package kbgt.generation

import kbgt.logic._
import scala.collection._
import scala.collection.mutable.ListBuffer
import Array._

/** A directed defeasible clash graph.
  *
  * @constructor
  *   create a new defeasible clash graph with sets of atoms, conflict
  *   relations, and defeasible relations.
  * @param A
  *   a set of atoms
  * @param C
  *   a set of conflict relations
  * @param D
  *   a set of defeasible relations
  */
class DCG(
    A: mutable.Set[Atom],
    C: mutable.Set[ConflictRelation],
    D: mutable.Set[DefeasibleRelation]
) {

  /** The DDGKB algorithm. */
  def DDGKB(defeasibleOnly: Boolean): MixedKnowledgeBase = {
    var atomGenerator = new AtomGenerator(A)
    var kb = new MixedKnowledgeBase()
    for (ConflictRelation(a, b) <- C) {
      val contradictionAtom = atomGenerator.generateAtom()
      kb +=
        (if (!defeasibleOnly) BinCon(BinOp.Implies, a, b)
         else DefeasibleFormula(a, b))
      kb += DefeasibleFormula(a, contradictionAtom)
      kb += DefeasibleFormula(b, UnCon(UnOp.Not, contradictionAtom))
    }
    for (DefeasibleRelation(a, b) <- D)
      kb += DefeasibleFormula(a, b)
    return kb
  }

  /** The Warshall algorithm. */
  def Warshall(
      atomSet: mutable.Set[Atom],
      defeasibleLinks: mutable.Set[DefeasibleRelation]
  ) = {
    var M = ofDim[Int](atomSet.size, atomSet.size)
    var index = 0
    var indexMap = mutable.Map[Atom, Int]()
    for (atom <- A) {
      indexMap(atom) = index
      index += 1
    }
    for (DefeasibleRelation(a, b) <- defeasibleLinks)
      M(indexMap(a))(indexMap(b)) = 1
    for (k <- 0 to atomSet.size - 1) {
      for (j <- 0 to atomSet.size - 1) {
        for (i <- 0 to atomSet.size - 1) {
          if (M(j)(i) == 0) {
            M(j)(i) = if ((M(j)(k) != 0) && (M(k)(i) != 0)) 1 else 0
          }
        }
      }
    }

    var result = mutable.Set[(Atom, Atom)]()
    for (a <- atomSet)
      for (b <- atomSet) {
        if (M(indexMap(a))(indexMap(b)) == 1) {
          result += ((a, b))
        }
      }

    result
  }

  /** The SimplifyDCG algorithm. */
  def SimplifyDCG() = {
    var newC = C.clone()
    for ((a, b) <- Warshall(A, D)) {
      for (ConflictRelation(y, d) <- C) {
        newC += (ConflictRelation(a, d))
      }
    }
    new SimpleDCG(A, newC)
  }

  /** Topological sort algorithm. */
  def Topological(
      b: mutable.Set[Atom],
      c: mutable.Set[ConflictRelation]
  ): List[Atom] = {
    var result = ListBuffer[Atom]()
    if (c.isEmpty) {
      result.addOne(b.head)
      return result.toList
    }
    val atomMap: Map[Atom, Atom] =
      c.map(con => con.antecedent -> con.consequent).toMap
    var probe =
      atomMap.keySet.filter(k => !atomMap.values.toList.contains(k)).head
    result += probe
    while (atomMap.keySet.contains(probe)) {
      probe = atomMap(probe)
      result += probe
    }
    result.reverse.toList
  }

  /** The ChainDCGKB algorithm. */
  def ChainDCGKB(defeasibleOnly: Boolean) = {
    var atomGenerator = new AtomGenerator(A)
    var kb = new MixedKnowledgeBase()
    var i = 0
    var B = A.diff(D.map(d => d.antecedent))
    val atomsOrdered = Topological(B, C)
    val contraAtom = atomGenerator.generateAtom()
    while (i < B.size) {
      if (i > 0)
        kb +=
          (if (!defeasibleOnly)
             BinCon(BinOp.Implies, atomsOrdered(i), atomsOrdered(i - 1))
           else DefeasibleFormula(atomsOrdered(i), atomsOrdered(i - 1)))
      if ((i + 1) % 2 == 0)
        kb += DefeasibleFormula(atomsOrdered(i), contraAtom)
      else
        kb += DefeasibleFormula(atomsOrdered(i), UnCon(UnOp.Not, contraAtom))
      i += 1
    }
    for (DefeasibleRelation(a, b) <- D)
      kb += DefeasibleFormula(a, b)
    kb
  }

}
