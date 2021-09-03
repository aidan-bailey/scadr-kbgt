package skbgen.defeasiblelogic.baserank

import skbgen.logic._
import scala.collection.mutable.ListBuffer

object BaseRank {
  def rank(
      dkb: DefeasibleKnowledgeBase,
      ckb: ClassicalKnowledgeBase
  ): List[ClassicalKnowledgeBase] = {
    var mkb: ClassicalKnowledgeBase = dkb.materialize()

    var rankedKB = new ListBuffer[ClassicalKnowledgeBase]()
    var currentMaterialization = mkb

    var flag = true
    while (flag) {
      var prevMaterialization = currentMaterialization
      currentMaterialization = new ClassicalKnowledgeBase(List())

      var temp = prevMaterialization.union(ckb)

      for (f <- prevMaterialization) {
        f match {
          case BinCon(BinOp.Implies, l, r) =>
            if (temp.entails(l.negate()))
              currentMaterialization = currentMaterialization.incl(f)
          case otherwise => {}
        }
      }
      prevMaterialization =
        prevMaterialization.difference(currentMaterialization)
      if (currentMaterialization.size == 0) {
        rankedKB += prevMaterialization
        currentMaterialization = currentMaterialization.union(ckb)
        rankedKB += currentMaterialization
        flag = false
      } else if (prevMaterialization.size == 0) {
        currentMaterialization = currentMaterialization.union(ckb)
        rankedKB += currentMaterialization
        flag = false
      } else rankedKB += prevMaterialization
    }
    return rankedKB.toList
  }
}
