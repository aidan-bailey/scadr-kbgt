package kbgt

import java.io.FileNotFoundException
import org.tweetyproject.commons.ParserException
import kbgt.logic._
import kbgt.generation._
import kbgt._
import java.io.PrintWriter
import java.io.File

object Interactive {

  var unranked = true
  var helpString = ""

  def generateStyle(
      kb: MixedKnowledgeBase,
      ranks: Int,
      states: Int,
      distributionFunction: (Int, Int, Int) => Int,
      defeasibleOnly: Boolean
  ): Boolean = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println(
      "Select style: \n- (n)ormal\n- (c)conservative\n- (b)ack"
    )
    print("> ")
    var in = scala.io.StdIn.readLine()
    in match {
      case "b" => return false
      case "n" => {
        var (newCKB, newDKB) = KBGenerator
          .RankedGenerate(
            ranks,
            states,
            distributionFunction,
            defeasibleOnly
          )
          .getKnowledgeBases()
        helpString = "Generated knowledge base."
        kb.clear()
        kb ++= newDKB
        kb ++= newCKB
        unranked = true
        return true
      }
      case "c" => {
        var (newCKB, newDKB) = KBGenerator
          .ConservativeRankedGenerate(
            ranks,
            states,
            distributionFunction,
            defeasibleOnly
          )
          .getKnowledgeBases()
        helpString = "Generated knowledge base using conservative style."
        kb.clear()
        kb ++= newDKB
        kb ++= newCKB
        unranked = true
        return true
      }
      case _ =>
        helpString = s"'$in' is not a valid option."
    }
    return generateStyle(
      kb,
      ranks,
      states,
      distributionFunction,
      defeasibleOnly
    )
  }

  def generateType(
      kb: MixedKnowledgeBase,
      ranks: Int,
      state: Int,
      distributionFunction: (Int, Int, Int) => Int
  ): Boolean = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println(
      "Select type: \n- (m)ixed\n- (d)defeasibleOnly\n- (b)ack"
    )
    print("> ")
    var in = scala.io.StdIn.readLine()
    in match {
      case "m" => {
        if (generateStyle(kb, ranks, state, distributionFunction, false))
          return true
      }
      case "d" => {
        if (generateStyle(kb, ranks, state, distributionFunction, true))
          return true
      }
      case "b" => {
        return false
      }
      case _ =>
        helpString = s"'$in' is not a valid option."
    }
    return generateType(kb, ranks, state, distributionFunction)
  }

  def generateDistribution(
      kb: MixedKnowledgeBase,
      ranks: Int,
      states: Int
  ): Boolean = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println(
      "Select distribution: \n- (1) Uniform\n- (2) Exponential\n- (3) Normal\n- (4) Inverted-Exponential\n- (5) Inverted-Normal\n- (b)ack"
    )
    print("> ")
    var in = scala.io.StdIn.readLine()
    in match {
      case "1" => {
        if (generateType(kb, ranks, states, DistributionFunction.Uniform))
          return true
      }
      case "2" => {
        if (generateType(kb, ranks, states, DistributionFunction.Exponential))
          return true
      }
      case "3" => {
        if (generateType(kb, ranks, states, DistributionFunction.Normal))
          return true
      }
      case "4" => {
        if (
          generateType(
            kb,
            ranks,
            states,
            DistributionFunction.InvertedExponential
          )
        )
          return true
      }
      case "5" => {
        if (
          generateType(
            kb,
            ranks,
            states,
            DistributionFunction.InvertedNormal
          )
        )
          return true
      }
      case "b" => return false
      case _   => helpString = s"'$in' is not a valid option"
    }
    return generateDistribution(kb, ranks, states)
  }

  def generateStatements(
      kb: MixedKnowledgeBase,
      ranks: Int
  ): Boolean = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println(
      "Enter number of defeasible statements [or back]: "
    )
    print("> ")
    var in = scala.io.StdIn.readLine()
    if (in.equals("back"))
      return false
    try {
      var states = Integer.parseInt(in)
      if (generateDistribution(kb, ranks, states))
        return true
    } catch {
      case e: NumberFormatException =>
        helpString = s"'$in' is not a valid option."
    }
    return generateStatements(kb, ranks)
  }

  def generate(kb: MixedKnowledgeBase): Unit = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println(
      "Enter number of defeasible ranks [or back]: "
    )
    print("> ")
    var in = scala.io.StdIn.readLine()
    if (in.equals("back"))
      return ()
    try {
      var ranks = Integer.parseInt(in)
      if (generateStatements(kb, ranks))
        return ()
    } catch {
      case e: NumberFormatException =>
        helpString = s"'$in' is not a valid option."
    }
    return generate(kb)
  }

  def removeStatement(kb: MixedKnowledgeBase): Unit = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println(
      "Enter statement [or back]: "
    )
    print("> ")
    var in = scala.io.StdIn.readLine()
    if (in.equals("back"))
      return ()
    if (in.contains("~>")) {
      try {
        val pos = in.indexOf("~>")
        val ante =
          Parser.parseClassicalFormula(in.substring(0, pos))
        val desc =
          Parser.parseClassicalFormula(in.substring(pos + 2))
        kb -= DefeasibleFormula(ante, desc)
        helpString = s"Added defeasible statement '$in'."
        unranked = true
        return ()
      } catch {
        case e: ParserException =>
          helpString = s"Parsing error for defeasible statement '$in'."
      }
    } else {
      try {
        var formula = Parser.parseClassicalFormula(in)
        kb -= formula
        helpString = s"Added propositional statement '$in'."
        unranked = true
        return ()
      } catch {
        case e: ParserException =>
          helpString = s"Parsing error for classical statement '$in'."
      }
    }
    return removeStatement(kb)
  }

  def loadClassical(kb: MixedKnowledgeBase): Boolean = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println("Enter file name [or back]: ")
    print("> ")
    var in = scala.io.StdIn.readLine()
    if (in.equals("back"))
      return false
    try {
      kb ++= new ClassicalKnowledgeBase().loadFile(in + ".json")
      unranked = true
      helpString = s"Classical statements from file '${in + ".json"}' loaded."
      return true
    } catch {
      case e: FileNotFoundException =>
        helpString = s"File '${in + ".json"}' not found."
      case e: ParserException =>
        helpString = s"Parsing error in file '${in + ".json"}'."
    }
    return loadClassical(kb)
  }

  def loadDefeasible(kb: MixedKnowledgeBase): Boolean = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println("Enter file name [or back]: ")
    print("> ")
    var in = scala.io.StdIn.readLine()
    if (in.equals("back"))
      return false
    try {
      kb ++= new DefeasibleKnowledgeBase()
        .loadFile(in + ".json")
      unranked = true
      helpString = s"Defeasible statements from file '${in + ".json"}' loaded."
      return true
    } catch {
      case e: FileNotFoundException =>
        helpString = s"File '${in + ".json"}' not found."
      case e: ParserException =>
        helpString = s"Parsing error in file '${in + ".json"}'."
    }
    return loadDefeasible(kb)
  }

  def loadMixed(kb: MixedKnowledgeBase): Boolean = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println("Enter file name [or back]: ")
    print("> ")
    var in = scala.io.StdIn.readLine()
    if (in.equals("back"))
      return false
    try {
      kb.loadFile(in + ".json")
      unranked = true
      helpString = s"All statements from file '${in + ".json"}' loaded."
      return true
    } catch {
      case e: FileNotFoundException =>
        helpString = s"File '${in + ".json"}' not found."
      case e: ParserException =>
        helpString = s"Parsing error in file '${in + ".json"}'."
    }
    return loadMixed(kb)
  }

  def loadRanked(
      kb: MixedKnowledgeBase,
      rankedKB: RankedKnowledgeBase
  ): Boolean = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println("Enter file name [or back]: ")
    print("> ")
    var in = scala.io.StdIn.readLine()
    if (in.equals("back"))
      return false
    try {
      rankedKB.readFile(in + ".json")
      val mkb = rankedKB.getMixedKnowledgeBase()
      kb.clear()
      kb ++= mkb.getClassical
      kb ++= mkb.getDefeasible
      unranked = false
      helpString = s"Ranked knowledge base from file '${in + ".json"}' loaded."
      return true
    } catch {
      case e: FileNotFoundException =>
        helpString = s"File '${in + ".json"}' not found."
      case e: ParserException =>
        helpString = s"Parsing error in file '${in + ".json"}'."
    }
    return loadRanked(kb, rankedKB)
  }

  def loadFile(
      kb: MixedKnowledgeBase,
      rankedKB: RankedKnowledgeBase
  ): Unit = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println(
      "Select load type:\n- (c)lassical knowledge base\n- (d)efeasible knowledge base\n- (m)ixed knowledge base\n- (r)anked knowledge base\n- (b)ack"
    )
    print("> ")
    val in = scala.io.StdIn.readLine()
    in match {
      case "c" =>
        if (loadClassical(kb))
          return ()
      case "d" =>
        if (loadDefeasible(kb))
          return ()
      case "m" =>
        if (loadMixed(kb))
          return ()
      case "r" =>
        if (loadRanked(kb, rankedKB))
          return ()
      case "b" => return ()
      case _   => helpString = s"'${in}' is not a valid option."
    }
    return loadFile(kb, rankedKB)
  }

  def saveFile(kb: MixedKnowledgeBase, rankedKB: RankedKnowledgeBase): Unit = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println(
      "Select save type:\n- (c)lassical knowledge base\n- (d)efeasible knowledge base\n- (m)ixed knowledge base\n- (r)anked knowledge base\n- (b)ack"
    )
    print("> ")
    var in = scala.io.StdIn.readLine()
    in match {
      case "b" => {
        return ()
      }
      case "c" => {
        if (!helpString.isEmpty()) println(helpString)
        helpString = ""
        println("Enter file name [or back]: ")
        print("> ")
        in = scala.io.StdIn.readLine()
        if (!in.equals("back")) {
          kb.getClassical.writeFile(in + ".json")
          helpString = s"Classical statements written to ${in + ".json"}."
          return ()
        }
      }
      case "d" => {
        if (!helpString.isEmpty()) println(helpString)
        helpString = ""
        println("Enter file name [or back]: ")
        print("> ")
        in = scala.io.StdIn.readLine()
        if (!in.equals("back")) {
          kb.getDefeasible.writeFile(in + ".json")
          helpString = s"Defeasible statements written to ${in + ".json"}."
          return ()
        }
      }
      case "m" => {
        if (writeSCADR(kb))
          return ()
      }
      case "r" =>
        if (writeRanked(kb, rankedKB))
          return ()
      case _ => helpString = s"'${in}' is not a valid option."
    }
    return saveFile(kb, rankedKB)
  }

  def writeSCADR(kb: MixedKnowledgeBase): Boolean = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println("Write in SCADR format? (y/n/b): ")
    print("> ")
    var in = scala.io.StdIn.readLine()
    in match {
      case "y" => {
        println("Enter file name [or back]: ")
        print("> ")
        in = scala.io.StdIn.readLine()
        if (!in.equals("back")) {
          kb.writeScadrFile(in + ".txt")
          helpString =
            s"All statements written to ${in + ".txt"} using SCADR format."
          return true
        }
      }
      case "n" => {
        println("Enter file name [or back]: ")
        print("> ")
        in = scala.io.StdIn.readLine()
        if (!in.equals("back")) {
          kb.writeFile(in + ".json")
          helpString = s"All statements written to ${in + ".json"}."
          return true
        }
      }
      case "b" =>
        return false
      case _ => helpString = s"'${in}' is not a valid option."
    }
    return writeSCADR(kb)
  }

  def writeRanked(
      kb: MixedKnowledgeBase,
      rankedKB: RankedKnowledgeBase
  ): Boolean = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println("Enter file name [or back]: ")
    print("> ")
    var in = scala.io.StdIn.readLine()
    if (in.equals("back"))
      return false
    if (unranked)
      rankedKB.replace(kb.baseRank())
    rankedKB.writeFile(in + ".json")
    helpString = s"Ranked knowledge base written to ${in + ".json"}."
    return true
  }

  def addStatement(kb: MixedKnowledgeBase): Unit = {
    if (!helpString.isEmpty()) println(helpString)
    helpString = ""
    println(
      "Enter statement (&&,||,=>,<=>,!,~>) [or back]: "
    )
    print("> ")
    var in = scala.io.StdIn.readLine()
    if (in.equals("back"))
      return ()
    if (in.contains("~>")) {
      try {
        val pos = in.indexOf("~>")
        val ante =
          Parser.parseClassicalFormula(in.substring(0, pos))
        val desc =
          Parser.parseClassicalFormula(in.substring(pos + 2))
        kb += DefeasibleFormula(ante, desc)
        helpString = s"Added defeasible statement ${in}."
        unranked = true
        return ()
      } catch {
        case e: ParserException =>
          helpString = s"Parsing error for defeasible statement $in."
      }
    } else {
      try {
        var formula = Parser.parseClassicalFormula(in)
        kb += formula
        helpString = s"Added classical statement ${in}."
        unranked = true
        return ()
      } catch {
        case e: ParserException =>
          helpString = s"Parsing error for classical statement $in."
      }
    }
    return addStatement(kb)
  }

  def knowledgebaseREPL(
      kb: MixedKnowledgeBase
  ) = {
    var flag = true
    var rankedKB = kb.baseRank()
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
      println("Classical: " + kb.getClassical)
      println("Defeasible: " + kb.getDefeasible)
      if (!helpString.isEmpty()) println(helpString)
      helpString = ""
      for (item <- menu)
        println(item)
      print("> ")
      var in = scala.io.StdIn.readLine()
      in match {
        case "g" => generate(kb)
        case "a" => addStatement(kb)
        case "r" => removeStatement(kb)
        case "s" => saveFile(kb, rankedKB)
        case "l" => loadFile(kb, rankedKB)
        case "c" => {
          kb.clear()
          rankedKB.clear()
        }
        case "p" => {
          if (unranked) {
            rankedKB = kb.baseRank()
            unranked = false
          }
          println(rankedKB)
        }
        case "q" => flag = false
        case _   => helpString = s"'${in}' is not a valid option."
      }

    }
  }

}
