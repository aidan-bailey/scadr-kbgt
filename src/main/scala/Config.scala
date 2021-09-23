package kbgt

import kbgt.generation.DistributionFunction

case class Config(
    rankCount: Int = -1,
    stateCount: Int = -1,
    filename: String = "",
    interactive: Boolean = false,
    conservative: Boolean = false,
    defeasible: Boolean = false,
    scadrFile: Boolean = false,
    distributionFunction: (Int, Int, Int) => Int = DistributionFunction.Uniform
)
