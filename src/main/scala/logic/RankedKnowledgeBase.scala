package skbgen.logic

import skbgen._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable

class RankedKnowledgeBase(rkb: List[ClassicalKnowledgeBase])
    extends mutable.ArrayStack[ClassicalKnowledgeBase] {

  def this(ckbs: ClassicalKnowledgeBase*) = {
    this(ckbs.toList)
  }

  def prettyPrint(): Unit = {
    println("---------------------------------------------------------------")
    println(" RANK | STATES")
    println("---------------------------------------------------------------")
    for (rankIndex <- 0 to rkb.size - 1) {
      print(
        if (rkb.size == rankIndex + 1) "     ∞|"
        else f"$rankIndex%6s|"
      )
      val states = rkb(rankIndex).toList
      var buffer = ""
      for (stateIndex <- 0 to states.size - 1) {
        val stateStr = states(stateIndex).toString()
        if (buffer.size + 1 + stateStr.size > 55) {
          print(buffer)
          buffer = ""
          println()
          print("      |")
        }
        buffer = buffer + ' ' + stateStr
      }
      print(buffer)
      println(
        "\n---------------------------------------------------------------"
      )
    }
  }

  def mixedKnowledgeBase: MixedKnowledgeBase = {
    var ckb = new ClassicalKnowledgeBase
    for (newCKB <- rkb.init)
      ckb ++= newCKB.clone()
    var dkb = new DefeasibleKnowledgeBase
    for (formula <- rkb.last.clone()) {
      formula match {
        case BinCon(BinOp.Implies, leftOperand, rightOperand) =>
          dkb += DefeasibleImplication(leftOperand, rightOperand)
        case _ => ()
      }
    }
    return new MixedKnowledgeBase(ckb, dkb)
  }

  def writeFile(filename: String) = {
    val pw = new PrintWriter(new File(filename + ".ranked"))
    for (rank <- rkb) {
      var buffer = ""
      for (state <- rank)
        buffer = buffer + state.tweety().toString() + ','
      buffer = buffer.substring(0, buffer.size - 1)
      pw.write(buffer + "\n")
    }
    pw.close()
  }

  def addDefeasible(formula: DefeasibleImplication) = {
    var atomSet = mutable.Set[Atom]()
    atomSet.addAll(formula.atoms)
    var stateSet = mutable.Set[Formula]()
    stateSet += formula.materialize()
    // check classical
    var flag = true
    while (flag) {
      flag = false
      // check defeasible ranks
      for (rank <- rkb.init) {
        var newStates = mutable.Set[Formula]()
        for (state <- rank) {
          val atoms = state.atoms()
          if (atoms.intersect(atomSet).nonEmpty) {
            atomSet.addAll(atoms)
            newStates.add(state)
            flag = true
          }
        }
        // remove identified states
        rank --= newStates
      }
      // check classical rank
      for (state <- rkb.last) {
        val atoms = state.atoms()
        if (atoms.intersect(atomSet).nonEmpty) {
          atomSet.addAll(atoms)
        }
      }
    }
  }

}
