package skbgen.kbgenerator

import skbgen.logic._
import skbgen.config._
import java.io._
import scala.util.Random

object KBGenerator {
  def outputfile(
      filename: String,
      dkb: DefeasibleKnowledgeBase,
      ckb: KnowledgeBase
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
  ): (DefeasibleKnowledgeBase, KnowledgeBase) = {
    def verbosePrint(message: String) = if (cfg.verbose) println(message)
    var nodeID: BigInt = 0
    var dkb = new DefeasibleKnowledgeBase(List())
    var ckb = new KnowledgeBase(List())
    var contraAtom: Formula = Atom(nodeID.toString())
    nodeID += 1
    var rootAtom = Atom(nodeID.toString())
    nodeID += 1
    dkb = dkb.incl(DefeasibleImplication(rootAtom, contraAtom))
    verbosePrint(s"Added rank 1/${cfg.rankCount}")
    var current = rootAtom
    var prevRanks = 0
    val stateCount = cfg.meanStates * cfg.rankCount
    for (rankNo <- 1 to cfg.rankCount - 1) {
      contraAtom = contraAtom.negate()
      var atom = Atom(nodeID.toString())
      nodeID += 1
      var ranks = DistributionOption.getStateCount(cfg, rankNo)
      if (cfg.statementOption.equals(StatementOption.Defeasible))
        ranks = ranks / 2
      for (rankIndex <- 0 to ranks - 1) {
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
          s"Added statement ${rankIndex + 1}/${ranks} to rank ${rankNo + 1}"
        )
        if (rankIndex != ranks - 1) {
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
