package skbgen.logic

import skbgen.config._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable

class MixedKnowledgeBase(
    ckb: ClassicalKnowledgeBase,
    dkb: DefeasibleKnowledgeBase
) {

  def this() = this(new ClassicalKnowledgeBase, new DefeasibleKnowledgeBase)
  def classical: ClassicalKnowledgeBase = ckb
  def defeasible: DefeasibleKnowledgeBase = dkb

  def writeFile(filename: String) = {
    val pw = new PrintWriter(new File(filename + ".kb"))
    if (ckb.isEmpty && dkb.isEmpty)
      pw.close()
    else if (ckb.isEmpty)
      pw.write(dkb.toParseString())
    else if (dkb.isEmpty)
      pw.write(ckb.toParseString())
    else
      pw.write(ckb.toParseString() + '\n' + dkb.toParseString())
    pw.close()
  }

  override def toString(): String = {
    ckb.toString() + '\n' + dkb.toString()
  }

  def clear(): Unit = {
    ckb.clear()
    dkb.clear()
  }

  def rank(): RankedKnowledgeBase = {
    if (ckb.isEmpty && dkb.isEmpty)
      return new RankedKnowledgeBase
    var mkb: ClassicalKnowledgeBase = dkb.materialize()
    var rankedKB = new ListBuffer[ClassicalKnowledgeBase]()
    var currentMaterialization = mkb

    var flag = true
    while (flag) {
      var prevMaterialization = currentMaterialization
      currentMaterialization = new ClassicalKnowledgeBase()
      var temp = prevMaterialization ++ ckb
      for (f <- prevMaterialization) {
        f match {
          case BinCon(BinOp.Implies, l, r) =>
            if (temp.entails(l.negate()))
              currentMaterialization += f
          case otherwise => {}
        }
      }
      prevMaterialization --= currentMaterialization
      if (currentMaterialization.size == 0) {
        rankedKB += prevMaterialization
        currentMaterialization ++= ckb
        rankedKB += currentMaterialization
        flag = false
      } else if (prevMaterialization.size == 0) {
        currentMaterialization ++= ckb
        rankedKB += currentMaterialization
        flag = false
      } else rankedKB += prevMaterialization
    }
    return new RankedKnowledgeBase(rankedKB.toList)
  }

}
