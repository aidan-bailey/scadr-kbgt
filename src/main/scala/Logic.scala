package skbgen.logic

sealed trait Operation extends Enumeration

/** Binary operator enums and maps. */
object BinOp extends Operation {

  /** Binary operator enums. */
  val And, Or, Implies, Iff = Value

  /** Maps to symbolic representation. */
  val symbolMap: Map[BinOp.Value, String] =
    Map(
      And -> "& ",
      Or -> "| ",
      Implies -> ">",
      Iff -> "="
    )
  /*
    Map(
      And -> "\\land ",
      Or -> "\\vee ",
      Implies -> "\\rightarrow ",
      Iff -> "\\leftrightarrow "
    )
   */
  //Map(And -> "∧", Or -> "∨", Implies -> "=>", Iff -> "↔")
  //Map(AND -> "∧", OR -> "∨", IMPLIES -> "→", IFF -> "↔")

  /** Maps to function. */
  val functionMap: Map[BinOp.Value, (Boolean, Boolean) => Boolean] =
    Map(
      And -> ((p: Boolean, q: Boolean) => p && q),
      Or -> ((p: Boolean, q: Boolean) => p || q),
      Implies -> ((p: Boolean, q: Boolean) => p <= q),
      Iff -> ((p: Boolean, q: Boolean) => p == q)
    )
}

/** Unary operator enums and maps. */
object UnOp extends Operation {

  /** Unary operator enums. */
  val Not = Value

  /** Maps to symbolic representation. */
  val symbolMap: Map[UnOp.Value, String] =
    //Map(Not -> "¬")
    Map(Not -> "!")
  //Map(Not -> "\\neg ")
  //Map(Not -> "~")

  /** Maps to function. */
  val functionMap: Map[UnOp.Value, Boolean => Boolean] = Map(
    Not -> ((p: Boolean) => !p)
  )

}

/** Defines a propositional formula. */
sealed abstract trait Formula {

  // Abstract Methods

  /** Evaluates a propositional formula. */
  def evaluate(valuation: Map[String, Boolean]): Boolean

  /** Returns the set of atoms. */
  def atoms(): Set[String]

  /** Returns the formula's negation. */
  def negate(): Formula

  /** toString override. */
  override def toString(): String

  // Concrete Methods

  /** Returns the set of possible valuations. */
  def world(): Set[Map[String, Boolean]] = {
    var atomSeq = atoms().toSeq
    var atomCount = atomSeq.size
    var valuations: Set[Map[String, Boolean]] = Set()
    var bound = math.pow(2, atomCount).toInt
    for (i <- 0 to bound - 1) {
      var valuation: Map[String, Boolean] = Map()
      var binString = i.toBinaryString
      binString = "0" * (atomCount - binString.length()) + binString
      for (j <- 0 to atomCount - 1) {
        var assignment = binString(j) == '1'
        var atom = atomSeq(j)
        valuation += (atom -> assignment)
      }
      valuations += valuation
    }
    return valuations
  }

  /** Returns the set of models. */
  def models(): Set[Map[String, Boolean]] = world().filter(v => evaluate(v))

  /** Checks entailment. */
  def entails(formula: Formula): Boolean = models().subsetOf(formula.models())

  /** Checks validity. */
  def isValid(): Boolean = models().size == math.pow(2, atoms().size)

}

/** Represents a propositional atom. */
case class Atom(name: String) extends Formula {

  /** Evaluates a propositional formula. */
  override def evaluate(valuation: Map[String, Boolean]): Boolean = valuation(
    name
  )

  /** Returns the set of atoms. */
  override def atoms(): Set[String] = Set(name)

  /** Returns the formula's negation. */
  override def negate(): Formula = UnCon(UnOp.Not, this)

  /** toString override. */
  override def toString(): String = name

}

/** Represents a propositional constant. */
case class Const(value: Boolean) extends Formula {

  /** Evaluates a propositional constant. */
  override def evaluate(valuation: Map[String, Boolean]): Boolean =
    value

  /** Returns the set of atoms. */
  override def atoms(): Set[String] = Set()

  /** Returns the formula's negation. */
  override def negate(): Formula = UnCon(UnOp.Not, this)

  /** toString override. */
  override def toString(): String = if (value) "+" else "-"

}

/** Represents a propositional binary connective. */
case class BinCon(
    operator: BinOp.Value,
    leftOperand: Formula,
    rightOperand: Formula
) extends Formula {

  /** Evaluates a propositional binary connective. */
  override def evaluate(valuation: Map[String, Boolean]): Boolean = {
    return BinOp.functionMap(operator)(
      leftOperand.evaluate(valuation),
      rightOperand.evaluate(valuation)
    )
  }

  /** Returns the set of atoms. */
  override def atoms(): Set[String] = {
    return leftOperand.atoms() ++ rightOperand.atoms()
  }

  /** Returns the formula's negation. */
  override def negate(): Formula = UnCon(UnOp.Not, this)

  /** toString override. */
  override def toString(): String = {
    return "(" + leftOperand.toString() + BinOp.symbolMap(
      operator
    ) + rightOperand.toString() + ")"
  }

}

/** Represents a propositional unary connective. */
case class UnCon(operator: UnOp.Value, operand: Formula) extends Formula {

  /** Evaluates a propositional unary connective. */
  override def evaluate(valuation: Map[String, Boolean]): Boolean = {
    return UnOp.functionMap(operator)(operand.evaluate(valuation));
  }

  /** Returns the set of atoms. */
  override def atoms(): Set[String] = {
    return operand.atoms()
  }

  /** toString override. */
  override def toString(): String = {
    return "(" + UnOp.symbolMap(operator) + operand
      .toString() + ")";
  }

  /** Returns the formula's negation. */
  override def negate(): Formula = {
    return operand;
  }

}

/** Represents a propositional knowledge base. */
class ClassicalKnowledgeBase(kb: Seq[Formula]) extends Set[Formula] {

  /** Returns the set of models. */
  def models(): Set[Map[String, Boolean]] = {
    var iter = iterator
    if (iter.isEmpty)
      return Set()
    var modelSet: Set[Map[String, Boolean]] = iter.next().models();
    while (iter.hasNext)
      modelSet = modelSet.filter(iter.next().models())
    return modelSet
  }

  /** Checks entailment. */
  def entails(formula: Formula): Boolean = models().subsetOf(formula.models())

  /** Disjunct knowledge bases. */
  def union(newKB: ClassicalKnowledgeBase): ClassicalKnowledgeBase = {
    var newSeq: Seq[Formula] = kb
    for (f <- newKB.iterator) {
      if (!contains(f)) {
        newSeq = newSeq :+ f
      }
    }
    return new ClassicalKnowledgeBase(newSeq)
  }

  /** Conjunct knowledge bases. */
  def intersection(
      newKB: ClassicalKnowledgeBase
  ): ClassicalKnowledgeBase = {
    var newSeq: Seq[Formula] = kb
    for (f <- newKB.iterator) {
      if (contains(f)) {
        newSeq = newSeq :+ f
      }
    }
    return new ClassicalKnowledgeBase(newSeq)
  }

  /** Conjunct knowledge bases. */
  def difference(
      newKB: ClassicalKnowledgeBase
  ): ClassicalKnowledgeBase = {
    var newSeq: Seq[Formula] = Seq()
    for (f <- kb) {
      if (!newKB.contains(f)) {
        newSeq = newSeq :+ f
      }
    }
    return new ClassicalKnowledgeBase(newSeq)
  }

  // Set Overrides
  override def iterator: Iterator[Formula] = kb.iterator
  override def contains(formula: Formula): Boolean = kb.contains(formula)
  override def incl(formula: Formula): ClassicalKnowledgeBase =
    if (contains(formula)) this
    else new ClassicalKnowledgeBase(kb :+ formula)
  override def excl(formula: Formula): ClassicalKnowledgeBase =
    new ClassicalKnowledgeBase(
      kb.filterNot(_ == formula)
    )

}

case class DefeasibleImplication(p: Formula, q: Formula) {
  override def toString(): String =
    p.toString() + "~>" + q.toString()
  def materialize(): Formula = BinCon(BinOp.Implies, p, q)
}

/** Represents a propositional knowledge base. */
class DefeasibleKnowledgeBase(kb: Seq[DefeasibleImplication])
    extends Set[DefeasibleImplication] {

  def materialize(): ClassicalKnowledgeBase =
    new ClassicalKnowledgeBase(for (f <- kb) yield f.materialize())

  // Set Overrides
  override def iterator: Iterator[DefeasibleImplication] = kb.iterator
  override def contains(defImpl: DefeasibleImplication): Boolean =
    kb.contains(defImpl)
  override def incl(defImpl: DefeasibleImplication): DefeasibleKnowledgeBase =
    if (contains(defImpl)) this else new DefeasibleKnowledgeBase(kb :+ defImpl)
  override def excl(defImpl: DefeasibleImplication): DefeasibleKnowledgeBase =
    new DefeasibleKnowledgeBase(
      kb.filterNot(_ == defImpl)
    )

}
