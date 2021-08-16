package skbgen.kbgenerator

import skbgen.logic._
import java.io._
import scala.util.Random

case class Config(
    rankCount: Int = -1,
    maxStates: Int = -1,
    filename: String = "",
    verbose: Boolean = false,
    statementOption: StatementOption.Value = StatementOption.MaxClassical,
    distributionOption: DistributionOption.Value = DistributionOption.Linear
)

object DistributionOption extends Enumeration {
  val Linear, Exponential, Peak, Trough, Random = Value
}

object StatementOption extends Enumeration {
  val Defeasible, Mixed, MaxClassical = Value
}

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
    var random = new Random()
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
    for (rankNo <- 1 to cfg.rankCount - 1) {
      contraAtom = contraAtom.negate()
      var atom = Atom(nodeID.toString())
      nodeID += 1
      var ranks = cfg.distributionOption match {
        case DistributionOption.Linear => cfg.maxStates
        case DistributionOption.Exponential =>
          math
            .ceil(
              cfg.maxStates * math
                .pow(rankNo.toDouble / cfg.rankCount.toDouble, 2)
            )
            .toInt
        case DistributionOption.Peak =>
          math
            .ceil(
              cfg.maxStates * math
                .sin(math.Pi * rankNo.toDouble / cfg.rankCount.toDouble)
            )
            .toInt
        case DistributionOption.Trough =>
          math
            .ceil(
              cfg.maxStates * (-math
                .sin(math.Pi * rankNo.toDouble / cfg.rankCount.toDouble) + 1)
            )
            .toInt
        case DistributionOption.Random =>
          random.nextInt(cfg.maxStates - 1) + 1
      }
      if (cfg.statementOption.equals(StatementOption.Defeasible))
        ranks = ranks / 2
      for (rankIndex <- 0 to ranks - 1) {
        if (
          cfg.statementOption.equals(
            StatementOption.MaxClassical
          ) || (cfg.statementOption
            .equals(StatementOption.Mixed)
            && random.nextBoolean())
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
