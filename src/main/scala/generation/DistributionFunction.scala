package kbgt.generation

/** A collection of distribution functions. */
object DistributionFunction {

  /** The erf function. */
  private def erf(x: Double): Double = {
    val sign = if (x < 0) -1 else 1
    val absx = math.abs(x)

    val t = 1.0 / (1.0 + 0.3275911 * absx);
    val y =
      1.0 - (((((1.061405429 * t + -1.453152027) * t) + 1.421413741) * t + -0.284496736) * t + 0.254829592) * t * math
        .exp(
          -x * x
        );
    sign * y
  }

  /** The uniform distribution function.
    * @param s
    *   total state count
    * @param r
    *   total defeasible rank count
    * @param x
    *   current defeasible rank number
    */
  def Uniform(s: Int, r: Int, x: Int) =
    math.round(s.toDouble / r.toDouble).toInt

  /** The exponential distribution function.
    *
    * @param s
    *   total state count
    * @param r
    *   total defeasible rank count
    * @param x
    *   current defeasible rank number
    */
  def Exponential(s: Int, r: Int, x: Int) = Math
    .round(
      s * (math.pow(math.E, 1 - (5 * (x.toDouble - 1) / r.toDouble)) - math
        .pow(math.E, -(math.pow(x.toDouble, 5) / r.toDouble)))
    )
    .toInt

  /** The normal distribution function.
    * @param s
    *   total state count
    * @param r
    *   total defeasible rank count
    * @param x
    *   current defeasible rank number
    */
  def Normal(s: Int, r: Int, x: Int) =
    math
      .round(
        s * ((erf(4 * x.toDouble / r.toDouble - 2) - erf(
          4 * (x - 1) / r.toDouble - 2
        )) / 2)
      )
      .toInt

  /** The inverted exponential distribution function.
    *
    * @param s
    *   total state count
    * @param r
    *   total defeasible rank count
    * @param x
    *   current defeasible rank number
    */
  def InvertedExponential(s: Int, r: Int, x: Int) = Math
    .round(
      s * (math.pow(math.E, 1 - (5 * (r - x.toDouble) / r.toDouble)) - math
        .pow(math.E, -(math.pow((r - x.toDouble + 1), 5) / r.toDouble)))
    )
    .toInt

  /** The inverted normal function.
    *
    * @param s
    *   total state count
    * @param r
    *   total defeasible rank count
    * @param x
    *   current defeasible rank number
    */
  def InvertedNormal(s: Int, r: Int, x: Int) =
    if (x < (r.toDouble + 1) / 2)
      Normal(s, r, x + math.round((r + 1) / 2).toInt)
    else
      Normal(s, r, x - math.round((r + 1) / 2).toInt)

}
