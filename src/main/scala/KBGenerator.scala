package skbgen.kbgenerator

import skbgen.logic._
import skbgen.config._
import java.io._
import scala.util.Random

object KBGenerator {

  def outputfile(
      filename: String,
      dkb: DefeasibleKnowledgeBase,
      ckb: ClassicalKnowledgeBase
  ): Unit = {
    val pw = new PrintWriter(new File(filename))
    for (p <- ckb)
      pw.write(p.toString().substring(1, p.toString().size - 1) + "\n")
    for (p <- dkb)
      pw.write(p.toString() + "\n")
    pw.close
  }

  def generate(
      cfg: Config
  ): (DefeasibleKnowledgeBase, ClassicalKnowledgeBase) = {

    return (
      new DefeasibleKnowledgeBase(List()),
      new ClassicalKnowledgeBase(List())
    )
  }

  def generateStructural(
      cfg: Config
  ): (DefeasibleKnowledgeBase, ClassicalKnowledgeBase) = {
    def verbosePrint(message: String) = if (cfg.verbose) println(message)
    var nodeID: BigInt = 0
    var dkb = new DefeasibleKnowledgeBase(List())
    var ckb = new ClassicalKnowledgeBase(List())
    var contraAtom: Formula = Atom(nodeID.toString())
    nodeID += 1
    var states = DistributionOption.getStateCount(cfg, 1)
    for (rankIndex <- 0 to states - 2) {
      var rootAtom = Atom(nodeID.toString())
      nodeID += 1
      dkb = dkb.incl(DefeasibleImplication(rootAtom, contraAtom))
      verbosePrint(
        s"Added statement ${rankIndex + 1}/${states} to rank 0"
      )
    }
    var rootAtom = Atom(nodeID.toString())
    nodeID += 1
    dkb = dkb.incl(DefeasibleImplication(rootAtom, contraAtom))
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
        if (
          cfg.statementOption.equals(
            StatementOption.MaxClassical
          ) || (cfg.statementOption
            .equals(StatementOption.Mixed)
            && DistributionOption.random.nextBoolean())
        )
          ckb = ckb.incl(BinCon(BinOp.Implies, atom, current))
        else
          dkb = dkb.incl(DefeasibleImplication(atom, current))
        dkb = dkb.incl(DefeasibleImplication(atom, contraAtom))
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
