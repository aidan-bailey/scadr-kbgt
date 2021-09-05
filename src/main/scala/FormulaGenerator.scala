package skbgen.formulagenerator
import skbgen.logic._
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics._

sealed abstract trait GenericFormula {

  def toFormula(
      queue: Queue[BinOp.Value],
      atomAssignment: List[String]
  ): Formula

  def getBinCount(): Int

  def getAtomCount(): Int

  def getComponents(): List[GenericFormula]

  override def toString(): String
}

case class GenBin(leftOperand: GenericFormula, rightOperand: GenericFormula)
    extends GenericFormula {

  def toFormula(
      queue: Queue[BinOp.Value],
      atomAssignment: List[String]
  ): Formula = {
    var op = queue.dequeue()
    return BinCon(
      op,
      leftOperand.toFormula(queue, atomAssignment),
      rightOperand.toFormula(queue, atomAssignment)
    )
  }

  def getBinCount(): Int =
    1 + leftOperand.getBinCount() + rightOperand.getBinCount()

  def getAtomCount(): Int =
    0 + leftOperand.getAtomCount() + rightOperand.getAtomCount()

  def getComponents(): List[GenericFormula] = {
    List(leftOperand, rightOperand)
  }

  override def toString(): String = {
    "(" + leftOperand.toString() + "⋅" + rightOperand.toString() + ")"
  }

}

case class GenAtom(name: Int, negated: Boolean) extends GenericFormula {
  def toFormula(
      queue: Queue[BinOp.Value],
      atomAssignment: List[String]
  ): Formula =
    return if (negated) UnCon(UnOp.Not, Atom(atomAssignment(name)))
    else Atom(atomAssignment(name))
  def getBinCount(): Int = 0
  def getAtomCount(): Int = 1
  def getComponents(): List[GenericFormula] = {
    List(this)
  }
  override def toString(): String = {
    if (negated) UnCon(UnOp.Not, Atom(name.toString())).toString()
    else Atom(name.toString()).toString()
  }
}

object FormulaGenerator {

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

  def generatePossible(
      f: GenericFormula,
      atomList: List[String]
  ): Set[Formula] = {
    var result: ListBuffer[Formula] = new ListBuffer()
    val sequ = BinOp.values.toSeq
    val opCount = f.getBinCount()
    val limit: Int = math.pow(sequ.length, opCount).toInt
    for (i <- 0 to limit - 1) {
      var baseStr = dec2baseStr(i, sequ.length)
      if (baseStr.length() < opCount)
        baseStr = "0" * (opCount - baseStr.length) + baseStr
      else if (baseStr.length() > opCount)
        baseStr = baseStr.drop(baseStr.length - opCount)
      var queue = new Queue[BinOp.Value]()
      for (char <- baseStr.toCharArray()) {
        var op = sequ(char.asDigit)
        queue.enqueue(op)
      }
      var formula = f.toFormula(queue, atomList)
      result += formula
    }
    return result.toList.toSet
  }

  def generateParameterized(
      kb: ClassicalKnowledgeBase,
      f: GenericFormula,
      atomList: List[String]
  ): Set[Formula] = {
    var result: ListBuffer[Formula] = new ListBuffer()
    val sequ = BinOp.values.toSeq
    val opCount = f.getBinCount()
    val limit: Int = math.pow(sequ.length, opCount).toInt
    for (i <- 0 to limit - 1) {
      var baseStr = dec2baseStr(i, sequ.length)
      if (baseStr.length() < opCount)
        baseStr = "0" * (opCount - baseStr.length) + baseStr
      else if (baseStr.length() > opCount)
        baseStr = baseStr.drop(baseStr.length - opCount)
      var queue = new Queue[BinOp.Value]()
      for (char <- baseStr.toCharArray()) {
        var op = sequ(char.asDigit)
        queue.enqueue(op)
      }
      var formula = f.toFormula(queue, atomList)
      if (
        !kb.contains(formula) && formula.models.nonEmpty && kb.entails(formula)
      )
        result += formula
    }
    return result.toList.toSet
  }

  def generateAll(
      formulas: Set[GenericFormula],
      atomList: List[String]
  ): Set[Formula] = {
    var result: ListBuffer[Formula] = new ListBuffer()
    for (f <- formulas) {
      result = result ++ generatePossible(f, atomList).toList
    }
    return result.toSet
  }

  def generateFormula(
      f: GenericFormula,
      atomList: List[String]
  ): Set[Formula] = {
    var result: ListBuffer[Formula] = new ListBuffer()
    val sequ = BinOp.values.toSeq
    val opCount = f.getBinCount()
    val limit: Int = math.pow(sequ.length, opCount).toInt
    for (i <- 0 to limit - 1) {
      var baseStr = dec2baseStr(i, sequ.length)
      if (baseStr.length() < opCount)
        baseStr = "0" * (opCount - baseStr.length) + baseStr
      else if (baseStr.length() > opCount)
        baseStr = baseStr.drop(baseStr.length - opCount)
      var queue = new Queue[BinOp.Value]()
      for (char <- baseStr.toCharArray()) {
        var op = sequ(char.asDigit)
        queue.enqueue(op)
      }
      var formula = f.toFormula(queue, atomList)
      result += formula
    }
    return result.toList.toSet
  }

  def entailedBy(kb: ClassicalKnowledgeBase): Set[Formula] = {
    val atoms = kb.atoms()
    var result = ListBuffer[Formula]()
    for (atomCount <- 1 to atoms.size) {
      val generics = generateGenerics(atomCount)
      for (atomList <- atoms.toList.combinations(atomCount)) {
        for (formula <- generics) {
          result ++= generateParameterized(kb, formula, atomList)
        }
      }
    }
    return result.toSet
  }

}
