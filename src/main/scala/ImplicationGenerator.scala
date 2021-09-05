package skbgen.implicationgenerator

import scala.collection.mutable.Queue
import skbgen.logic._
import scala.collection.mutable.ListBuffer
import skbgen.formulagenerator._

object ImplicationGenerator {

  private def dec2baseStr(num: Int, base: Int): String = {
    if (num >= (base - 1)) {
      return dec2baseStr(num / base, base) + (num % base).toString()
    } else
      return (num % base).toString()
  }

  private def power[A](s: List[A]): List[List[A]] = {
    def pwr(s: List[A], acc: List[List[A]]): List[List[A]] = s match {
      case Nil     => acc
      case a :: as => pwr(as, acc ::: (acc map (a :: _)))
    }
    pwr(s, Nil :: Nil)
  }

  def generateGenerics(atoms: Int): Set[GenericFormula] = {
    var result = ListBuffer[GenericFormula]()
    def generate(
        parent: List[GenericFormula]
    ): List[GenericFormula] = {
      if (parent.size == 1) {
        return parent
      }
      var result = new ListBuffer[GenericFormula]()
      var sets = parent.toList.combinations(2)
      for (s <- sets) {
        val left = s(0)
        val right = s(1)
        var bin = GenBin(left, right)
        var fullSet =
          List(bin) ++ parent diff List(left, right)
        for (p <- fullSet.permutations) {
          result = result ++ generate(p)
        }
      }
      result.toList
    }
    val limit: Int = math.pow(2, atoms).toInt
    for (i <- 0 to limit - 1) {
      var baseStr = dec2baseStr(i, 2)
      if (baseStr.length() < atoms)
        baseStr = "0" * (atoms - baseStr.length) + baseStr
      else if (baseStr.length() > atoms)
        baseStr = baseStr.drop(baseStr.length - atoms)
      var buffer = new ListBuffer[GenericFormula]()
      for (j <- 0 to atoms - 1) {
        buffer += GenAtom(j, baseStr(j).asDigit > 0)
      }
      result = result ++ generate(buffer.toList)
    }
    return result.toSet
  }

  def generateImplication(mkb: ClassicalKnowledgeBase) = {
    val atoms = mkb.atoms()
    var result = ListBuffer[Formula]()
    //for (anteCount <- 1 to atoms.size) {
    for (anteCount <- 1 to 1) {
      val anteGenerics = generateGenerics(anteCount)
      for (anteAtoms <- atoms.toList.combinations(anteCount)) {
        for (
          antecedent <- FormulaGenerator.generateAll(anteGenerics, anteAtoms)
        ) {
          //for (descCount <- 1 to atoms.size) {
          for (descCount <- 1 to 1) {
            val descGenerics = generateGenerics(descCount)
            for (descAtoms <- atoms.toList.combinations(descCount)) {
              for (
                descendent <- FormulaGenerator.generateAll(
                  descGenerics,
                  descAtoms
                )
              ) {
                val formula = BinCon(BinOp.Implies, antecedent, descendent)
                val temp = mkb.incl(formula)
                if (
                  formula.models.nonEmpty && temp.entails(antecedent.negate())
                )
                  println(formula)
              }
            }
          }
        }
      }
    }
    //generate antecedent
    //generate descendent
  }

}
