import skbgen.logic._
import skbgen.kbgenerator._
import scopt._

object Main extends App {

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("kbgen"),
      head("kbgen", "1.0"),
      opt[Int]('c', "count")
        .required()
        .action((x, c) => c.copy(rankCount = x))
        .text("rank count"),
      opt[Int]('a', "average")
        .required()
        .action((x, c) => c.copy(avgRankLength = x))
        .text("average rank size"),
      opt[String]('s', "statements")
        .valueName("<opt>")
        .action((x, c) =>
          x match {
            case "classical" =>
              c.copy(statementOption = StatementOption.MaxClassical)
            case "defeasible" =>
              c.copy(statementOption = StatementOption.Defeasible)
            case "mixed" =>
              c.copy(statementOption = StatementOption.Mixed)
          }
        )
        .text("statement option {classical, defeasible, mixed}"),
      opt[String]('d', "distribution")
        .valueName("<opt>")
        .action((x, c) =>
          x match {
            case "linear" =>
              c.copy(distributionOption = DistributionOption.Linear)
            case "exponential" =>
              c.copy(distributionOption = DistributionOption.Exponential)
            case "peak" =>
              c.copy(distributionOption = DistributionOption.Peak)
            case "trough" =>
              c.copy(distributionOption = DistributionOption.Trough)
            case "random" =>
              c.copy(distributionOption = DistributionOption.Random)
          }
        )
        .text(
          "distribution option {linear, exponential, peak, trough, random}"
        ),
      opt[String]('o', "out")
        .valueName("<filename>")
        .action((x, c) => c.copy(filename = x))
        .text("output file name"),
      opt[Unit]("verbose")
        .action((_, c) => c.copy(verbose = true))
        .text("enable trace statements"),
      help("help").text("prints this usage text")
    )
  }

  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      println("Generating knowledge base...")
      var (dkb, ckb) = KBGenerator.generate(config)
      println("Knowledge base generation complete.")
      println("Writing to file...")
      if (config.filename.equals("")) {
        KBGenerator.outputfile("out.txt", dkb, ckb)
        println(s"Knowledge base written to out.txt.")
      } else {
        KBGenerator.outputfile(config.filename, dkb, ckb)
        println(s"Knowledge base written to ${config.filename}.")
      }
    case _ =>
    // arguments are bad, error message will have been displayed
  }
}
