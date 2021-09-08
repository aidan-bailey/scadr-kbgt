import skbgen.logic._
import skbgen.generation._
import skbgen._
import scopt._
import org.tweetyproject.logics.pl.parser.PlParser
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.commons.ParserException
import java.io.File
import java.io.PrintWriter
import java.io.FileNotFoundException

object Main extends App {
  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("kbgen"),
      head("kbgen", "1.0"),
      opt[Int]('r', "ranks")
        .action((x, c) => c.copy(rankCount = x))
        .text("total number of ranks"),
      opt[Int]('s', "states")
        .action((x, c) => c.copy(stateCount = x))
        .text("total number of states"),
      opt[String]('t', "type")
        .valueName("<opt>")
        .action((x, c) =>
          x match {
            case "defeasible" =>
              c.copy(statementOption = StatementOption.Defeasible)
            case "mixed" =>
              c.copy(statementOption = StatementOption.Mixed)
          }
        )
        .text("statement type {defeasible, mixed}"),
      opt[String]('d', "distribution")
        .valueName("<opt>")
        .action((x, c) =>
          x match {
            case "uniform" =>
              c.copy(distributionOption = DistributionOption.Uniform)
            case "exponential" =>
              c.copy(distributionOption = DistributionOption.Exponential)
            case "inverted-exponential" =>
              c.copy(distributionOption =
                DistributionOption.InvertedExponential
              )
            case "normal" =>
              c.copy(distributionOption = DistributionOption.Normal)
            case "inverted-normal" =>
              c.copy(distributionOption = DistributionOption.InvertedNormal)
          }
        )
        .text(
          "distribution option {uniform, exponential, normal, inverted-exponential, inverted-normal}"
        ),
      opt[String]('n', "notation")
        .valueName("<opt>")
        .action((x, c) =>
          x match {
            case "tweety" =>
              c.copy(notationOption = NotationOption.Tweety)
            case "formal" =>
              c.copy(notationOption = NotationOption.Formal)
            case "latex" =>
              c.copy(notationOption = NotationOption.Latex)
          }
        )
        .text(
          "notation option {tweety, formal, latex}"
        ),
      opt[String]('o', "out")
        .valueName("<filename>")
        .action((x, c) => c.copy(filename = x))
        .text("output file name"),
      opt[Unit]("verbose")
        .action((_, c) => c.copy(verbose = true))
        .text("enable trace statements"),
      opt[Unit]("interactive")
        .action((_, c) => c.copy(interactive = true))
        .text("run in interactive repl mode"),
      help("help").text("prints this usage text"),
      note("\n*First elements in option sets are defaults")
    )
  }

  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      Notation.choice = config.notationOption
      var kb = new MixedKnowledgeBase
      if (config.rankCount > 0 && config.stateCount > 0) {
        println("Generating knowledge base...")
        var (dkb, ckb) = config.statementOption match {
          case StatementOption.Defeasible =>
            KBGenerator.generateDefeasible(config)
          case StatementOption.Mixed =>
            KBGenerator.generateStructural(config)
        }
        kb = new MixedKnowledgeBase(ckb, dkb)
        println("Knowledge base generation complete.")
        if (config.filename.equals("")) {
          if (!config.interactive) {
            println("Classical :", ckb.toString())
            println("Defeasible :", dkb.toString())
          }
        } else {
          println("Writing to file...")
          KBGenerator.outputfile(config.filename, dkb, ckb)
          println(s"Knowledge base written to ${config.filename}.")
        }
      }
      if (config.interactive) {
        println("Entering interactive mode.")
        Interactive.knowledgebaseREPL(
          kb
        )
      }
    case _ =>
  }
}
