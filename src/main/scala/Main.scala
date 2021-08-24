import skbgen.logic._
import skbgen.kbgenerator._
import skbgen.config._
import scopt._

object Main extends App {

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("kbgen"),
      head("kbgen", "1.0"),
      opt[Int]('r', "ranks")
        .required()
        .action((x, c) => c.copy(rankCount = x))
        .text("number of ranks"),
      opt[Int]('s', "states")
        .required()
        .action((x, c) => c.copy(meanStates = x))
        .text("mean states per rank"),
      opt[String]('t', "type")
        .valueName("<opt>")
        .action((x, c) =>
          x match {
            case "max-classical" =>
              c.copy(statementOption = StatementOption.MaxClassical)
            case "defeasible" =>
              c.copy(statementOption = StatementOption.Defeasible)
            case "mixed" =>
              c.copy(statementOption = StatementOption.Mixed)
          }
        )
        .text("statement type {max-classical, defeasible, mixed}"),
      opt[String]('d', "distribution")
        .valueName("<opt>")
        .action((x, c) =>
          x match {
            case "linear" =>
              c.copy(distributionOption = DistributionOption.Linear)
            case "exponential" =>
              c.copy(distributionOption = DistributionOption.Exponential)
            case "normal" =>
              c.copy(distributionOption = DistributionOption.Normal)
            case "inverted-normal" =>
              c.copy(distributionOption = DistributionOption.InvertedNormal)
            case "random" =>
              c.copy(distributionOption = DistributionOption.Random)
          }
        )
        .text(
          "distribution option {linear, exponential, normal, inverted-normal, random}"
        ),
      opt[String]('n', "notation")
        .valueName("<opt>")
        .action((x, c) =>
          x match {
            case "tweety" =>
              c.copy(notationOption = NotationOption.Tweety)
            case "formal" =>
              c.copy(notationOption = NotationOption.Formal)
            case "simple" =>
              c.copy(notationOption = NotationOption.Simple)
            case "latex" =>
              c.copy(notationOption = NotationOption.Latex)
          }
        )
        .text(
          "notation option {tweety, formal, simple, latex}"
        ),
      opt[String]('o', "out")
        .valueName("<filename>")
        .action((x, c) => c.copy(filename = x))
        .text("output file name"),
      opt[Unit]("verbose")
        .action((_, c) => c.copy(verbose = true))
        .text("enable trace statements"),
      help("help").text("prints this usage text"),
      note("\n*First elements in option sets are defaults")
    )
  }

  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      BinOp.notation = config.notationOption
      UnOp.notation = config.notationOption
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
