package skbgen.generation

import skbgen.logic._
import skbgen._
import java.io._
import scala.util.Random

object KBGenerator {

  def outputfile(
      filename: String,
      dkb: DefeasibleKnowledgeBase,
      ckb: ClassicalKnowledgeBase
  ): Unit = {
    val pw = new PrintWriter(new File(filename + ".kb"))
    for (p <- ckb)
      pw.write(p.toParseString() + "\n")
    for (p <- dkb)
      pw.write(p.toParseString() + "\n")
    pw.close
  }

  def generateDefeasible(
      cfg: Config
  ): (DefeasibleKnowledgeBase, ClassicalKnowledgeBase) = {
    def verbosePrint(message: String) = if (cfg.verbose) println(message)
    var nodeID: BigInt = 0
    var dkb = new DefeasibleKnowledgeBase()
    var ckb = new ClassicalKnowledgeBase()
    var contraAtom: Formula = Atom(nodeID.toString())
    nodeID += 1
    var states = math.max(DistributionOption.getStateCount(cfg, 1), 2)
    var prevAtom: Formula = Atom(nodeID.toString())
    nodeID += 1
    dkb += DefeasibleImplication(prevAtom, contraAtom)
    verbosePrint(
      s"Added statement 1/${states} to rank 0"
    )
    for (rankIndex <- 0 to states - 2) {
      var atom = Atom(nodeID.toString())
      nodeID += 1
      dkb += DefeasibleImplication(atom, prevAtom)
      verbosePrint(
        s"Added statement ${rankIndex + 2}/${states} to rank 0"
      )
      prevAtom = atom
    }
    verbosePrint(s"Added rank 1/${cfg.rankCount}")
    for (rankNo <- 1 to cfg.rankCount - 1) {
      contraAtom = contraAtom.negate()
      var atom = Atom(nodeID.toString())
      nodeID += 1
      states = math.max(DistributionOption.getStateCount(cfg, rankNo + 1), 2)
      dkb += DefeasibleImplication(atom, prevAtom)
      verbosePrint(s"Added statement ${1}/${states} to rank ${rankNo}")
      dkb += DefeasibleImplication(atom, contraAtom)
      verbosePrint(s"Added statement ${2}/${states} to rank ${rankNo}")
      prevAtom = atom
      for (rankIndex <- 0 to states - 3) {
        var rootAtom = Atom(nodeID.toString())
        nodeID += 1
        dkb += DefeasibleImplication(rootAtom, prevAtom)
        verbosePrint(
          s"Added statement ${rankIndex + 1}/${states} to rank ${rankNo}"
        )
        prevAtom = rootAtom
      }
      verbosePrint(s"Added rank ${rankNo + 1}/${cfg.rankCount}")
    }
    return (dkb, ckb)
  }

  def generateStructural(
      cfg: Config
  ): (DefeasibleKnowledgeBase, ClassicalKnowledgeBase) = {
    def verbosePrint(message: String) = if (cfg.verbose) println(message)
    var nodeID: BigInt = 0
    var dkb = new DefeasibleKnowledgeBase()
    var ckb = new ClassicalKnowledgeBase()
    var contraAtom: Formula = Atom(nodeID.toString())
    nodeID += 1
    var states = DistributionOption.getStateCount(cfg, 1)
    for (rankIndex <- 0 to states - 2) {
      var rootAtom = Atom(nodeID.toString())
      nodeID += 1
      dkb += DefeasibleImplication(rootAtom, contraAtom)
      verbosePrint(
        s"Added statement ${rankIndex + 1}/${states} to rank 0"
      )
    }
    var rootAtom = Atom(nodeID.toString())
    nodeID += 1
    dkb += DefeasibleImplication(rootAtom, contraAtom)
    verbosePrint(
      s"Added statement ${states}/${states} to rank 0"
    )
    verbosePrint(s"Added rank 1/${cfg.rankCount}")
    var current = rootAtom
    for (rankNo <- 1 to cfg.rankCount - 1) {
      contraAtom = contraAtom.negate()
      var atom = Atom(nodeID.toString())
      nodeID += 1
      states = math.max(
        DistributionOption.getStateCount(cfg, rankNo + 1),
        1
      )
      for (rankIndex <- 0 to states - 1) {
        ckb += BinCon(BinOp.Implies, atom, current)
        dkb += DefeasibleImplication(atom, contraAtom)
        verbosePrint(
          s"Added statement ${rankIndex + 1}/${states} to rank ${rankNo}"
        )
        if (rankIndex != states - 1) {
          atom = Atom(nodeID.toString())
          nodeID += 1
        }
      }
      current = atom
      verbosePrint(s"Added rank ${rankNo + 1}/${cfg.rankCount}")
    }
    return (dkb, ckb)
  }

}
