package skbgen.formulagenerator
import skbgen.logic._
import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer

sealed abstract trait GenericFormula {

  def toFormula(queue: Queue[BinOp.Value]): Formula

  def getBinCount(): Int

  def getComponents(): List[GenericFormula]

  override def toString(): String
}

case class GenBin(leftOperand: GenericFormula, rightOperand: GenericFormula)
    extends GenericFormula {

  def toFormula(queue: Queue[BinOp.Value]): Formula = {
    var op = queue.dequeue()
    return BinCon(
      op,
      leftOperand.toFormula(queue),
      rightOperand.toFormula(queue)
    )
  }

  def getBinCount(): Int =
    1 + leftOperand.getBinCount() + rightOperand.getBinCount()

  def getComponents(): List[GenericFormula] = {
    List(leftOperand, rightOperand)
  }

  override def toString(): String = {
    "(" + leftOperand.toString() + "⋅" + rightOperand.toString() + ")"
  }

}

case class GenAtom(name: String, negated: Boolean) extends GenericFormula {
  def toFormula(queue: Queue[BinOp.Value]): Formula =
    return if (negated) UnCon(UnOp.Not, Atom(name)) else Atom(name)
  def getBinCount(): Int = 0
  def getComponents(): List[GenericFormula] = {
    List(this)
  }
  override def toString(): String = {
    if (negated) UnCon(UnOp.Not, Atom(name)).toString()
    else Atom(name).toString()
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

  def generateGenerics(atoms: List[String]): Set[GenericFormula] = {
    var result: ListBuffer[GenericFormula] = new ListBuffer()
    def generate(parent: List[GenericFormula]): List[GenericFormula] = {
      if (parent.size == 1) {
        return parent
      }
      var result: List[GenericFormula] = List()
      var sets = power(parent).filter(p => p.length == 2);
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
      result
    }
    val limit: Int = math.pow(2, atoms.length).toInt
    for (i <- 0 to limit - 1) {
      var baseStr = dec2baseStr(i, 2)
      if (baseStr.length() < atoms.length)
        baseStr = "0" * (atoms.length - baseStr.length) + baseStr
      else if (baseStr.length() > atoms.length)
        baseStr = baseStr.drop(baseStr.length - atoms.length)
      var buffer: ListBuffer[GenAtom] = new ListBuffer()
      for (j <- 0 to atoms.length - 1)
        buffer += GenAtom(atoms(j), baseStr(j).asDigit > 0)
      result = result ++ generate(buffer.toList)
    }
    return result.toSet
  }

  def generateAll(formulas: Set[GenericFormula]): Set[Formula] = {
    var result: ListBuffer[Formula] = new ListBuffer()
    for (f <- formulas) {
      result = result ++ generatePossible(f).toList
    }
    return result.toSet
  }

  def generatePossible(f: GenericFormula): Set[Formula] = {
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
      var formula = f.toFormula(queue)
      result += formula
    }
    return result.toList.toSet
  }

}
