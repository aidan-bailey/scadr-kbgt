package kbgt.logic

import kbgt._
import scala.collection.mutable.ListBuffer
import org.tweetyproject.logics.pl.syntax._
import org.tweetyproject.logics.pl.sat._
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import scala.jdk.CollectionConverters._
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable
import scala.io.Source

/** A mixed propositional knowledge base.
  *
  * Extends [[kbgt.logic.KnowledgeBase]]
  *
  * @constructor
  *   create a mixed knowledge base with a sequence of formulas
  * @param formulas
  *   the sequence of formulas.
  */
class MixedKnowledgeBase(formulas: Formula*)
    extends KnowledgeBase[Formula](
      mutable.Set[Formula](formulas: _*)
    ) {

  def this(ckb: ClassicalKnowledgeBase, dkb: DefeasibleKnowledgeBase) = {
    this(
      ckb
        .map(f => f.asInstanceOf[Formula])
        .union(dkb.map(f => f.asInstanceOf[Formula]))
        .toSeq: _*
    )
  }

  /** Gets the classical and defeasible knowledge bases.
    *
    * @return
    *   the classical and defeasible knowledge bases
    */
  def getKnowledgeBases(): (ClassicalKnowledgeBase, DefeasibleKnowledgeBase) = {
    var ckb = new ClassicalKnowledgeBase
    var dkb = new DefeasibleKnowledgeBase
    iterator.foreach(formula =>
      if (formula.isInstanceOf[ClassicalFormula])
        ckb += formula.asInstanceOf[ClassicalFormula]
      else
        dkb += formula.asInstanceOf[DefeasibleFormula]
    )
    return (ckb, dkb)
  }

  /** Gets the classical statements in the mixed knowledge base.
    *
    * @return
    *   the classical knowledge base containing the classical statements.
    */
  def getClassical() =
    new ClassicalKnowledgeBase(
      iterator
        .filter(formula => formula.isInstanceOf[ClassicalFormula])
        .map(formula => formula.asInstanceOf[ClassicalFormula])
        .toSeq: _*
    )

  /** Gets the defeasible statements in the mixed knowledge base.
    *
    * @return
    *   the defeasible knowledge base containing the defeasible statements.
    */
  def getDefeasible() = new DefeasibleKnowledgeBase(
    iterator
      .filter(formula => formula.isInstanceOf[DefeasibleFormula])
      .map(formula => formula.asInstanceOf[DefeasibleFormula])
      .toSeq: _*
  )

  /** Gets the classical knowledge base containing the materialized statements
    * of the mixed knowledge base.
    *
    * @return
    *   the classical knowledge base contained the materialized statements.
    */
  def getMaterialization(): ClassicalKnowledgeBase =
    new ClassicalKnowledgeBase(
      iterator
        .map(f =>
          if (f.isInstanceOf[DefeasibleFormula])
            f.asInstanceOf[DefeasibleFormula].getMaterialization()
          else
            f.asInstanceOf[ClassicalFormula]
        )
        .toSeq: _*
    )

  /** Base Ranks the mixed knowledge base.
    *
    * @return
    *   the ranked knowledge base
    */
  def baseRank(): RankedKnowledgeBase = {
    val (ckb, dkb) = getKnowledgeBases
    if (ckb.isEmpty && dkb.isEmpty)
      return new RankedKnowledgeBase
    var rankedKB = new ListBuffer[MixedKnowledgeBase]()
    var currentMaterialization: MixedKnowledgeBase =
      new MixedKnowledgeBase(dkb.clone().toSeq: _*)
    var flag = true
    while (flag) {
      var prevMaterialization: MixedKnowledgeBase =
        currentMaterialization.clone()
      currentMaterialization = new MixedKnowledgeBase()
      var temp = (prevMaterialization ++ ckb).getMaterialization()
      for (f <- prevMaterialization) {
        val matf =
          if (f.isInstanceOf[DefeasibleFormula])
            f.asInstanceOf[DefeasibleFormula].getMaterialization()
          else
            f.asInstanceOf[ClassicalFormula]
        matf match {
          case BinCon(BinOp.Implies, l, _) =>
            if (temp.entails(UnCon(UnOp.Not, l)))
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
    return new RankedKnowledgeBase(rankedKB.init, rankedKB.last)
  }

  /** Loads statements from specified file into knowledge base.
    *
    * @param filename
    *   the name of the file
    */
  override def loadFile(filename: String): this.type = {
    val strings = Source
      .fromFile(filename)
      .getLines()
      .next()
      .init
      .tail
    if (!strings.isEmpty)
      addAll(
        strings
          .split(",")
          .map(string => Parser.parseFormula(string.init.tail))
      )
    this
  }

  /** Gets the TweetyProject counterpart for the mixed knowledge base.
    *
    * @return
    *   the PlBeliefSet counterpart of the mixed knowledge base
    */
  override def toPlBeliefSet(): PlBeliefSet = {
    var (ckb, dkb) = getKnowledgeBases()
    new ClassicalKnowledgeBase(
      ckb
        .map(f =>
          BinCon(
            BinOp.Implies,
            UnCon(UnOp.Not, f),
            Const(Constant.Contradiction)
          )
        )
        .toSeq: _*
    ).addAll(dkb.getMaterialization()).toPlBeliefSet()
  }

  /** clone override. */
  override def clone(): MixedKnowledgeBase =
    new MixedKnowledgeBase(iterator.toSeq: _*)

  /** toString override. */
  override def toString(): String = {
    val (ckb, dkb) = getKnowledgeBases()
    return s"Classical: ${ckb}\nDefeasible: ${dkb}"
  }

  def ++(kb: ClassicalKnowledgeBase) =
    clone().addAll(kb)

  def ++(kb: DefeasibleKnowledgeBase) =
    clone().addAll(kb)

  def --(kb: ClassicalKnowledgeBase) =
    clone().addAll(kb)

  def --(kb: DefeasibleKnowledgeBase) =
    clone().addAll(kb)

}
