package kbgt.logic

import kbgt._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable
import java.util.ArrayList
import scala.io.Source

/** A ranked knowledge base.
 *
 * @constructor
 *   create a new ranked knowledge base with a listbuffer of defeasible ranks and a mixed knowledge base infinite rank.
 * @param dRanks the defeasible ranks
 * @param iRanks the infinite rank
 */
class RankedKnowledgeBase(
    dRanks: ListBuffer[MixedKnowledgeBase],
    iRank: MixedKnowledgeBase
) {

  def this() = this(ListBuffer(), new MixedKnowledgeBase())

  /** Gets the defeasible ranks.
   *
   * @return the defeasible ranks
   */
  def defeasibleRanks(): ListBuffer[MixedKnowledgeBase] = dRanks

  /** Gets the infinite rank.
   *
   * @return the infinite rank
   */
  def infiniteRank(): MixedKnowledgeBase = iRank

  /** Replaces the current ranked knowledge base with a new one.
   *
   * @param rankedKB the new ranked knowledge base
   * */
  def replace(rankedKB: RankedKnowledgeBase): this.type = {
    dRanks.clear()
    dRanks.addAll(rankedKB.defeasibleRanks().clone())
    iRank.clear()
    iRank ++= rankedKB.infiniteRank().clone()
    this
  }

  /** Clear the ranked knowledge base. */
  def clear(): Unit = {
    dRanks.clear()
    iRank.clear()
  }

  /** Gets the defeasible rank count.
   *
   * @return the defeasible rank count
   */
  def rankCount() = dRanks.size

  /** Gets the mixed knowledge base.
   *
   * @return the mixed knowledge base
   */
  def getMixedKnowledgeBase(): MixedKnowledgeBase = {
    dRanks.foldLeft(new MixedKnowledgeBase())((mkb, f) => mkb ++= f).addAll(iRank)
  }

  def toPlBeliefSetArrayList(): ArrayList[PlBeliefSet] = {
    var result = new ArrayList[PlBeliefSet]()
    for (cbs <- dRanks.map(f => f.toPlBeliefSet())) {
      var beliefSet = new PlBeliefSet()
      beliefSet.addAll(cbs)
      result.add(beliefSet)
    }
    var cbs = iRank.toPlBeliefSet()
    var beliefSet = new PlBeliefSet()
    beliefSet.addAll(cbs)
    result.add(beliefSet)
    return result
  }

  /** Writes the ranked knowledge base to a file.
   *
   * @param filename the name of the file to output to.
   */
  def writeFile(filename: String) = {
    val pw = new PrintWriter(new File(filename))
    pw.write("[")
    for (rank <- dRanks)
        pw.write('['+rank.toParseString().split(",").map(f => s"\"$f\"").mkString(",")+"],")
    if (iRank.isEmpty)
      pw.write("[]")
    else
      pw.write('['+iRank.toParseString().split(",").map(f => s"\"$f\"").mkString(",")+']')
    pw.write("]")
    pw.close()
  }

  /** Read in a ranked knowledge base from a specified file.
   *
   * @param filename the filename of the specified file.
   */
  def readFile(filename: String): Unit = {
    var result = new RankedKnowledgeBase()
    val lines = Source.fromFile(filename).getLines().next().init.tail.split("\\],\\[").iterator
    var rank = new MixedKnowledgeBase
    while (lines.hasNext) {
      val line = lines.next().toString.replaceAll("\\[","").replaceAll("\\]","").replaceAll("\"", "")
      println(line)
      if (!line.isBlank())
        rank = Parser.parseString(line)
      if (lines.hasNext) {
        result.defeasibleRanks() += rank
      } else result.infiniteRank() ++= rank
    }
   clear()
   defeasibleRanks() ++= result.defeasibleRanks()
   infiniteRank() ++= result.infiniteRank()
  }

  /** toString override. */
  override def toString(): String = {
    var result = StringBuilder.newBuilder
    result.addAll(
      "\n---------------------------------------------------------------\n"
    )
    result.addAll(" RANK | STATES\n")
    result.addAll(
      "---------------------------------------------------------------\n"
    )
    for (rankIndex <- 0 to dRanks.size) {
      result.addAll(
        if (dRanks.size == rankIndex) "     ∞|"
        else f"$rankIndex%6s|"
      )
      val states =
        if (dRanks.size != rankIndex) dRanks(rankIndex).toList else iRank.toList
      var buffer = ""
      for (stateIndex <- 0 to states.size - 1) {
        val stateStr = states(stateIndex).toString()
        if (buffer.size + 1 + stateStr.size > 55) {
          result.addAll(buffer + "\n")
          buffer = ""
          println()
          result.addAll("      |")
        }
        buffer = buffer + ' ' + stateStr
      }
      result.addAll(buffer)
      result.addAll(
        "\n---------------------------------------------------------------\n"
      )
    }
    return result.toString()
  }
}
