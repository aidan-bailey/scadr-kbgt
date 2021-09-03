package skbgen.config

import scala.util.Random
import scala.collection.mutable.ListBuffer

//Calculates the error function using Horner’s method.
trait Erf {
  // constants
  val a1: Double = 0.254829592;
  val a2: Double = -0.284496736;
  val a3: Double = 1.421413741;
  val a4: Double = -1.453152027;
  val a5: Double = 1.061405429;
  val p: Double = 0.3275911;

  def erf(x: Double): Double = {
    val sign = if (x < 0) -1 else 1
    val absx = math.abs(x)

    val t = 1.0 / (1.0 + p * absx);
    val y =
      1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * math.exp(
        -x * x
      );
    sign * y
  }
}

private object Erf extends Erf

case class Config(
    rankCount: Int = -1,
    stateCount: Int = -1,
    filename: String = "",
    verbose: Boolean = false,
    inverted: Boolean = false,
    statementOption: StatementOption.Value = StatementOption.MaxClassical,
    distributionOption: DistributionOption.Value = DistributionOption.Uniform,
    notationOption: NotationOption.Value = NotationOption.Tweety
)

object NotationOption extends Enumeration {
  val Tweety, Latex, Simple, Formal = Value
}

object DistributionOption extends Enumeration {
  val Exponential, Normal, Uniform = Value
  val random = new Random()
  def getStateCount(cfg: Config, rankNo: Int): Int = {
    cfg.distributionOption match {
      case DistributionOption.Uniform => cfg.stateCount / cfg.rankCount
      case DistributionOption.Exponential =>
        Math
          .max(
            cfg.stateCount * (
              1 - math.pow(
                math.E,
                -(5 / cfg.rankCount.toDouble) * rankNo.toDouble
              )
            ),
            1
          )
          .toInt
      case DistributionOption.Normal =>
        math
          .max(
            cfg.stateCount * (((math.sqrt(math.Pi / 2) * Erf.erf(
              -4 / 2 + (8 / 2) * rankNo.toDouble / cfg.rankCount.toDouble
            ) + math.sqrt(math.Pi / 2)) / (math.sqrt(2) * math
              .sqrt(math.Pi))) - ((math.sqrt(math.Pi / 2) * Erf.erf(
              -4 / 2 + (8 / 2) * (rankNo.toDouble - 1) / cfg.rankCount.toDouble
            ) + math.sqrt(math.Pi / 2)) / (math.sqrt(2) * math
              .sqrt(math.Pi)))),
            1
          )
          .toInt
    }
  }
  def getStateQueue(cfg: Config): List[Int] = {
    var result = ListBuffer()

    var ls = result.toList
    ls.reverse
    return ls
  }
}

object StatementOption extends Enumeration {
  val Defeasible, Mixed, MaxClassical = Value
}
