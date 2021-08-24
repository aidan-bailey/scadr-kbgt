package skbgen.config

import scala.util.Random

case class Config(
    rankCount: Int = -1,
    meanStates: Int = -1,
    filename: String = "",
    verbose: Boolean = false,
    statementOption: StatementOption.Value = StatementOption.MaxClassical,
    distributionOption: DistributionOption.Value = DistributionOption.Linear,
    notationOption: NotationOption.Value = NotationOption.Tweety
)

object NotationOption extends Enumeration {
  val Tweety, Latex, Simple, Formal = Value
}

object DistributionOption extends Enumeration {
  val Linear, Exponential, Normal, InvertedNormal, Random = Value
  val random = new Random()
  def getStateCount(cfg: Config, rankNo: Int): Int = {
    val stateCount = cfg.rankCount * cfg.meanStates
    cfg.distributionOption match {
      case DistributionOption.Linear => cfg.meanStates
      case DistributionOption.Exponential =>
        math
          .ceil(
            cfg.meanStates * math
              .pow(rankNo.toDouble / cfg.rankCount.toDouble, 2)
          )
          .toInt
      case DistributionOption.Normal =>
        math
          .max(
            stateCount * math.pow(
              math.E,
              -math.pow(rankNo - (cfg.rankCount.toDouble / 2), 2) / (2 * math
                .pow((cfg.rankCount.toDouble / 2) / 4, 2))
            ) / (((cfg.rankCount.toDouble / 2) / 4) * math.sqrt(2 * math.Pi)),
            1
          )
          .toInt
      case DistributionOption.InvertedNormal =>
        math
          .max(
            stateCount * (math.pow(
              math.E,
              -math.pow(
                (cfg.rankCount.toDouble / 2).toInt - (cfg.rankCount.toDouble / 2),
                2
              ) / (2 * math
                .pow((cfg.rankCount.toDouble / 2) / 4, 2))
            ) / (((cfg.rankCount.toDouble / 2) / 4) * math
              .sqrt(2 * math.Pi)) - math.pow(
              math.E,
              -math.pow(rankNo - (cfg.rankCount.toDouble / 2), 2) / (2 * math
                .pow((cfg.rankCount.toDouble / 2) / 4, 2))
            ) / (((cfg.rankCount.toDouble / 2) / 4) * math
              .sqrt(2 * math.Pi))),
            1
          )
          .toInt
      case DistributionOption.Random =>
        random.nextInt(cfg.meanStates - 1) + 1
    }
  }
}

object StatementOption extends Enumeration {
  val Defeasible, Mixed, MaxClassical = Value
}
