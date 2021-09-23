package kbgt.generation

import kbgt.logic._
import scala.collection._
import scala.collection.mutable.ListBuffer
import Array._

/** A simple defeasible conflict graph.
  *
  * Extends [[DCG]]
  */
class SimpleDCG(
    A: mutable.Set[Atom],
    C: mutable.Set[ConflictRelation]
) extends DCG(A, C, mutable.Set[DefeasibleRelation]()) {

  /** The CompatibilityClosure algorithm. */
  def CompatibilityClosure() = {
    var M = ofDim[Float](A.size, A.size)
    for (j <- 0 to A.size - 1) {
      for (i <- 0 to A.size - 1) {
        M(j)(i) = Float.NegativeInfinity
      }
    }
    var index = 0
    var indexMap = mutable.Map[Atom, Int]()
    for (atom <- A) {
      indexMap(atom) = index
      index += 1
    }
    for (ConflictRelation(a, b) <- C) {
      M(indexMap(a))(indexMap(b)) = 1
    }
    for (k <- A.map(a => indexMap(a))) {
      for (j <- A.map(a => indexMap(a))) {
        for (i <- A.map(a => indexMap(a))) {
          if (M(i)(k) == Float.PositiveInfinity)
            M(j)(i) = Float.PositiveInfinity
          else if (M(i)(k) > 0 && M(k)(j) != Float.NegativeInfinity)
            M(j)(i) = math.max(M(j)(i), M(i)(k) + M(k)(j))
          if (j == i && M(j)(i) != Float.NegativeInfinity)
            M(j)(i) = Float.PositiveInfinity
        }
      }
    }
    var E = mutable.Set[(Atom, Float)]()
    for (a <- A) {
      var rank: Float = 0
      for (i <- M(indexMap(a))) {
        rank = math.max(rank, i)
      }
      E += ((a, rank))
    }
    E
  }

}
