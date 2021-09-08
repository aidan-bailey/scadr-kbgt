package skbgen

import java.io.FileNotFoundException
import org.tweetyproject.commons.ParserException
import skbgen.logic._
import skbgen.generation._
import skbgen._
import java.io.PrintWriter
import java.io.File

object Interactive {
  def knowledgebaseREPL(
      kb: MixedKnowledgeBase
  ) = {
    var flag = true
    var rankedKB = kb.rank()
    var menu = List(
      "Select an option:",
      "- (a)dd statement",
      "- (r)emove statement",
      "- (g)enerate knowledge base",
      "- (c)lear knowledge base",
      "- (p)rint ranked knowledge base",
      "- (s)ave to file",
      "- (l)oad from file",
      "- (q)uit"
    )
    var helpString = ""
    while (flag) {
      println("Classical: " + kb.classical)
      println("Defeasible: " + kb.defeasible)
      if (!helpString.isEmpty()) println(helpString)
      helpString = ""
      for (item <- menu)
        println(item)
      print("> ")
      var in = scala.io.StdIn.readLine()
      in match {
        case "g" => {
          while (flag) {
            if (!helpString.isEmpty()) println(helpString)
            helpString = ""
            println(
              "Enter number of ranks [or back]: "
            )
            print("> ")
            in = scala.io.StdIn.readLine()
            if (!in.equals("back"))
              try {
                var ranks = Integer.parseInt(in)
                while (flag) {
                  if (!helpString.isEmpty()) println(helpString)
                  helpString = ""
                  println(
                    "Enter number of statements [or back]: "
                  )
                  print("> ")
                  in = scala.io.StdIn.readLine()
                  if (!in.equals("back")) {
                    var states = Integer.parseInt(in)
                    while (flag) {
                      if (!helpString.isEmpty()) println(helpString)
                      helpString = ""
                      println(
                        "Select distribution: \n- (1) Uniform\n- (2) Exponential\n- (3) Normal\n- (4) Inverted-Exponential\n- (5) Inverted-Normal"
                      )
                      print("> ")
                      in = scala.io.StdIn.readLine()
                      in match {
                        case "1" => {
                          var (newDKB, newCKB) = KBGenerator.generateDefeasible(
                            new Config(
                              rankCount = ranks,
                              stateCount = states,
                              distributionOption = DistributionOption.Uniform
                            )
                          )
                          kb.clear()
                          kb.defeasible ++= newDKB
                          kb.classical ++= newCKB
                          rankedKB = kb.rank()
                          flag = false
                        }
                        case "2" => {
                          var (newDKB, newCKB) = KBGenerator.generateDefeasible(
                            new Config(
                              rankCount = ranks,
                              stateCount = states,
                              distributionOption =
                                DistributionOption.Exponential
                            )
                          )
                          kb.clear()
                          kb.defeasible ++= newDKB
                          kb.classical ++= newCKB
                          rankedKB = kb.rank()
                          flag = false
                        }
                        case "3" => {
                          var (newDKB, newCKB) = KBGenerator.generateDefeasible(
                            new Config(
                              rankCount = ranks,
                              stateCount = states,
                              distributionOption = DistributionOption.Normal
                            )
                          )
                          kb.clear()
                          kb.defeasible ++= newDKB
                          kb.classical ++= newCKB
                          rankedKB = kb.rank()
                          flag = false
                        }
                        case "4" => {
                          var (newDKB, newCKB) = KBGenerator.generateDefeasible(
                            new Config(
                              rankCount = ranks,
                              stateCount = states,
                              distributionOption =
                                DistributionOption.InvertedExponential
                            )
                          )
                          kb.clear()
                          kb.defeasible ++= newDKB
                          kb.classical ++= newCKB
                          rankedKB = kb.rank()
                          flag = false
                        }
                        case "5" => {
                          var (newDKB, newCKB) = KBGenerator.generateDefeasible(
                            new Config(
                              rankCount = ranks,
                              stateCount = states,
                              distributionOption =
                                DistributionOption.InvertedNormal
                            )
                          )
                          kb.clear()
                          kb.defeasible ++= newDKB
                          kb.classical ++= newCKB
                          rankedKB = kb.rank()
                          flag = false
                        }
                        case _ => helpString = s"'$in'"
                      }
                    }
                  } else
                    flag = false
                }
              } catch {
                case e: NumberFormatException =>
                  helpString = s"'$in' is not a valid option."
              }
            else
              flag = false
          }
          flag = true
        }
        case "a" => {
          while (flag) {
            if (!helpString.isEmpty()) println(helpString)
            helpString = ""
            println(
              "Enter statement [or back]: "
            )
            print("> ")
            in = scala.io.StdIn.readLine()
            if (!in.equals("back"))
              if (in.contains("~>")) {
                try {
                  val pos = in.indexOf("~>")
                  val ante =
                    Tweety.parseFormula(in.substring(0, pos))
                  val desc =
                    Tweety.parseFormula(in.substring(pos + 2))
                  kb.defeasible += DefeasibleImplication(ante, desc)
                  rankedKB = kb.rank()
                  flag = false
                } catch {
                  case e: ParserException =>
                    helpString = s"Parsing error for defeasible statement $in."
                }
              } else {
                try {
                  var formula = Tweety.parseFormula(in)
                  kb.classical += formula
                  rankedKB = kb.rank()
                  flag = false
                } catch {
                  case e: ParserException =>
                    helpString = s"Parsing error for classical statement $in."
                }
              }
            else
              flag = false
          }
          flag = true
        }
        case "r" => {
          while (flag) {
            if (!helpString.isEmpty()) println(helpString)
            helpString = ""
            println(
              "Enter statement [or back]: "
            )
            print("> ")
            in = scala.io.StdIn.readLine()
            if (!in.equals("back"))
              if (in.contains("~>")) {
                try {
                  val pos = in.indexOf("~>")
                  val ante =
                    Tweety.parseFormula(in.substring(0, pos))
                  val desc =
                    Tweety.parseFormula(in.substring(pos + 2))
                  kb.defeasible -= DefeasibleImplication(ante, desc)
                  rankedKB = kb.rank()
                  flag = false
                } catch {
                  case e: ParserException =>
                    helpString =
                      s"Parsing error for defeasible statement '$in'."
                }
              } else {
                try {
                  var formula = Tweety.parseFormula(in)
                  kb.classical -= formula
                  rankedKB = kb.rank()
                  flag = false
                } catch {
                  case e: ParserException =>
                    helpString = s"Parsing error for classical statement '$in'."
                }
              }
            else
              flag = false
          }
          flag = true
        }
        case "s" => {
          while (flag) {
            if (!helpString.isEmpty()) println(helpString)
            helpString = ""
            println(
              "Select save type:\n- (c)lassical knowledge base\n- (d)efeasible knowledge base\n- (m)ixed knowledge base\n- (r)anked knowledge base\n- (b)ack"
            )
            print("> ")
            in = scala.io.StdIn.readLine()
            in match {
              case "c" => {
                if (!helpString.isEmpty()) println(helpString)
                helpString = ""
                println("Enter file name [or back]: ")
                print("> ")
                in = scala.io.StdIn.readLine()
                if (!in.equals("back")) {
                  kb.classical.writeFile(in)
                  flag = false
                }
              }
              case "d" => {
                if (!helpString.isEmpty()) println(helpString)
                helpString = ""
                println("Enter file name [or back]: ")
                print("> ")
                in = scala.io.StdIn.readLine()
                if (!in.equals("back")) {
                  kb.defeasible.writeFile(in)
                  flag = false
                }
              }
              case "m" => {
                if (!helpString.isEmpty()) println(helpString)
                helpString = ""
                println("Enter file name [or back]: ")
                print("> ")
                in = scala.io.StdIn.readLine()
                if (!in.equals("back")) {
                  kb.writeFile(in)
                  flag = false
                }
              }
              case "r" => {
                if (!helpString.isEmpty()) println(helpString)
                helpString = ""
                println("Enter file name [or back]: ")
                print("> ")
                in = scala.io.StdIn.readLine()
                if (!in.equals("back")) {
                  rankedKB.writeFile(in)
                  flag = false
                }
              }
              case _ => helpString = s"'${in}' is not a valid option."
            }
          }
          flag = true
        }
        case "l" => {
          while (flag) {
            if (!helpString.isEmpty()) println(helpString)
            helpString = ""
            println(
              "Select load type:\n- (c)lassical knowledge base\n- (d)efeasible knowledge base\n- (m)ixed knowledge base\n- (r)anked knowledge base\n- (b)ack"
            )
            print("> ")
            in = scala.io.StdIn.readLine()
            in match {
              case "c" => {
                if (!helpString.isEmpty()) println(helpString)
                helpString = ""
                println("Enter file name [or back]: ")
                print("> ")
                in = scala.io.StdIn.readLine()
                if (!in.equals("back")) {
                  try {
                    kb.classical ++= Tweety.parseFile(in).classical
                    rankedKB = kb.rank()
                    println(
                      s"Classical statements from file '${in + ".kb"}' loaded."
                    )
                    flag = false
                  } catch {
                    case e: FileNotFoundException =>
                      helpString = s"File '${in + ".kb"}' not found."
                    case e: ParserException =>
                      helpString = s"Parsing error in file '${in + ".kb"}'."
                  }
                }
              }
              case "d" => {
                if (!helpString.isEmpty()) println(helpString)
                helpString = ""
                println("Enter file name [or back]: ")
                print("> ")
                in = scala.io.StdIn.readLine()
                if (!in.equals("back")) {
                  try {
                    kb.defeasible ++= Tweety.parseFile(in).defeasible
                    rankedKB = kb.rank()
                    println(
                      s"Defeasible statements from file '${in + ".kb"}' loaded."
                    )
                    flag = false
                  } catch {
                    case e: FileNotFoundException =>
                      helpString = s"File '${in + ".kb"}' not found."
                    case e: ParserException =>
                      helpString = s"Parsing error in file '${in + ".kb"}'."
                  }
                }
              }
              case "m" => {
                if (!helpString.isEmpty()) println(helpString)
                helpString = ""
                println("Enter file name [or back]: ")
                print("> ")
                in = scala.io.StdIn.readLine()
                if (!in.equals("back")) {
                  try {
                    var mixedKB = Tweety.parseFile(in)
                    kb.defeasible ++= mixedKB.defeasible
                    kb.classical ++= mixedKB.classical
                    rankedKB = kb.rank()
                    println(s"Statements from file '${in + ".kb"}' loaded.")
                    flag = false
                  } catch {
                    case e: FileNotFoundException =>
                      helpString = s"File '${in + ".kb"}' not found."
                    case e: ParserException =>
                      helpString = s"Parsing error in file '${in + ".kb"}'."
                  }
                }
              }
              case "r" => {
                if (!helpString.isEmpty()) println(helpString)
                helpString = ""
                println("Enter file name [or back]: ")
                print("> ")
                in = scala.io.StdIn.readLine()
                if (!in.equals("back")) {
                  try {
                    rankedKB = Tweety.parseRankedKB(in)
                    var kb = rankedKB.mixedKnowledgeBase
                    kb.clear
                    kb.classical ++= kb.classical
                    kb.defeasible ++= kb.defeasible
                    println(
                      s"Ranked knowledge base from file '${in + ".ranked"}' loaded."
                    )
                    flag = false
                  } catch {
                    case e: FileNotFoundException =>
                      helpString = s"File '${in + ".ranked"}' not found."
                    case e: ParserException =>
                      helpString = s"Parsing error in file '${in + ".ranked"}'."
                  }
                }
              }
              case "b" => flag = false
              case _   => helpString = s"'${in}' is not a valid option."
            }
          }
          flag = true
        }
        case "c" => {
          kb.clear()
          rankedKB = new RankedKnowledgeBase
        }
        case "p" => {
          rankedKB.prettyPrint()
        }
        case "q" => flag = false
        case _   => helpString = s"'${in}' is not a valid option."
      }

    }
  }

}
