import kbgt.logic._
import kbgt.generation._
import kbgt._
import scopt._
import scala.collection._
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
        .validate(x =>
          if (x > 0) success
          else failure("Value <ranks> must be >0")
        )
        .text("total number of defeasible ranks"),
      opt[Int]('s', "states")
        .action((x, c) => c.copy(stateCount = x))
        .validate(x =>
          if (x > 0) success
          else failure("Value <states> must be >0")
        )
        .text("total number of states"),
      opt[String]('d', "distribution")
        .valueName("<opt>")
        .action((x, c) =>
          x match {
            case "uniform" =>
              c.copy(distributionFunction = DistributionFunction.Uniform)
            case "exponential" =>
              c.copy(distributionFunction = DistributionFunction.Exponential)
            case "normal" =>
              c.copy(distributionFunction = DistributionFunction.Normal)
            case "inverted-exponential" =>
              c.copy(distributionFunction =
                DistributionFunction.InvertedExponential
              )
            case "inverted-normal" =>
              c.copy(distributionFunction = DistributionFunction.InvertedNormal)
          }
        )
        .text(
          "distribution option {uniform, exponential, normal, inverted-exponential, inverted-normal}"
        ),
      opt[Unit]("defeasibleOnly")
        .action((_, c) => c.copy(defeasible = true))
        .text("generate only defeasible statements"),
      opt[Unit]("conservative")
        .action((_, c) => c.copy(conservative = true))
        .text("use conservative form of generation"),
      opt[Unit]("scadrFile")
        .action((_, c) => c.copy(scadrFile = true))
        .text("use scadr file format"),
      opt[String]('o', "out")
        .valueName("<filename>")
        .action((x, c) => c.copy(filename = x))
        .text("output file name"),
      opt[Unit]("interactive")
        .action((_, c) => c.copy(interactive = true))
        .text("run in interactive repl mode"),
      help("help").text("prints this usage text"),
      note("\n*First elements in option sets are defaults")
    )
  }

  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      var kb = new MixedKnowledgeBase
      if (config.rankCount > 0 && config.stateCount > 0) {
        println("Generating knowledge base...")
        kb =
          if (config.conservative)
            KBGenerator.ConservativeRankedGenerate(
              config.rankCount,
              config.stateCount,
              config.distributionFunction,
              config.defeasible
            )
          else
            KBGenerator.RankedGenerate(
              config.rankCount,
              config.stateCount,
              config.distributionFunction,
              config.defeasible
            )
        println("Knowledge base generation complete.")
        if (config.filename.equals("")) {
          if (!config.interactive)
            println(kb)
        } else {
          println("Writing to file...")
          if (config.scadrFile)
            kb.writeScadrFile(config.filename + ".txt")
          else kb.writeFile(config.filename + ".json")
          println(s"Knowledge base written to ${config.filename + ".json"}.")
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
