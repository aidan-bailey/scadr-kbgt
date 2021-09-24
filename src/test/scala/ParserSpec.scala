package kbgt.test

import org.scalatest.funspec.AnyFunSpec
import kbgt.logic.Parser
import org.tweetyproject.logics.pl.syntax._
import kbgt.logic._

class ParserSpec extends AnyFunSpec {
  describe("The Parser can convert") {
    it("a Negation PlFormula to a Not UnCon") {
      val plFormula = new Negation(new Proposition("a"))
      val classicalFormula = new UnCon(UnOp.Not, Atom("a"))
      assert(classicalFormula.equals(Parser.tweety2formula(plFormula)))
    }
    it("a Conjunction PlFormula to an And BinCon") {
      val plFormula: PlFormula =
        new Conjunction(new Proposition("a"), new Proposition("b"))
      val classicalFormula: ClassicalFormula =
        BinCon(BinOp.And, Atom("a"), Atom("b"))
      assert(classicalFormula.equals(Parser.tweety2formula(plFormula)))
    }
    it("a Disjunction PlFormula to an Or BinCon") {
      val plFormula =
        new Disjunction(new Proposition("a"), new Proposition("b"))
      val classicalFormula = BinCon(BinOp.Or, Atom("a"), Atom("b"))
      assert(classicalFormula.equals(Parser.tweety2formula(plFormula)))
    }
    it("an Implication PlFormula to an Implies BinCon") {
      val plFormula =
        new Implication(new Proposition("a"), new Proposition("b"))
      val classicalFormula = BinCon(BinOp.Implies, Atom("a"), Atom("b"))
      assert(classicalFormula.equals(Parser.tweety2formula(plFormula)))
    }
    it("an Equivalence PlFormula to an Iff BinCon") {
      val plFormula =
        new Equivalence(new Proposition("a"), new Proposition("b"))
      val classicalFormula = BinCon(BinOp.Iff, Atom("a"), Atom("b"))
      assert(classicalFormula.equals(Parser.tweety2formula(plFormula)))
    }
    it("a Tautology PlFormula to a Tautology Constant") {
      val plFormula = new Tautology
      val classicalFormula = new Const(Constant.Tautology)
      assert(classicalFormula.equals(Parser.tweety2formula(plFormula)))
    }
    it("a Contradiction PlFormula to a Contradiction Constant") {
      val plFormula = new Contradiction
      val classicalFormula = new Const(Constant.Contradiction)
      assert(classicalFormula.equals(Parser.tweety2formula(plFormula)))
    }

  }
  describe("The Parser can parse") {
    it("a PlFormula") {
      val plFormulaParsed =
        Parser.parsePlFormula("((a || !b)=>((a&&d)<=>a))=>(+||-)")
      val plFormula = new Implication(
        new Implication(
          new Disjunction(
            new Proposition("a"),
            new Negation(new Proposition("b"))
          ),
          new Equivalence(
            new Conjunction(new Proposition("a"), new Proposition("d")),
            new Proposition("a")
          )
        ),
        new Disjunction(new Tautology, new Contradiction)
      )
      assert(plFormula.equals(plFormulaParsed))
    }
    it("a ClassicalFormula") {
      val classicalFormulaParsed =
        Parser.parseClassicalFormula("((a || !b)=>((a&&d)<=>a))=>(+||-)")
      val classicalFormula = BinCon(
        BinOp.Implies,
        BinCon(
          BinOp.Implies,
          BinCon(BinOp.Or, Atom("a"), UnCon(UnOp.Not, Atom("b"))),
          BinCon(BinOp.Iff, BinCon(BinOp.And, Atom("a"), Atom("d")), Atom("a"))
        ),
        BinCon(
          BinOp.Or,
          Const(Constant.Tautology),
          Const(Constant.Contradiction)
        )
      )
      assert(classicalFormula.equals(classicalFormulaParsed))
    }
    it("a DefeasibleFormula") {
      val defeasibleFormulaParsed =
        Parser.parseDefeasibleFormula("(a=>b)~>((b||!c)&&d)")
      val defeasibleFormula = DefeasibleFormula(
        BinCon(BinOp.Implies, Atom("a"), Atom("b")),
        BinCon(
          BinOp.And,
          BinCon(BinOp.Or, Atom("b"), UnCon(UnOp.Not, Atom("c"))),
          Atom("d")
        )
      )
      assert(defeasibleFormula.equals(defeasibleFormulaParsed))
    }
    it("a generic Formula (Classical)") {
      val formulaParsed =
        Parser.parseFormula("((a || !b)=>((a&&d)<=>a))=>(+||-)")
      val formula: Formula = BinCon(
        BinOp.Implies,
        BinCon(
          BinOp.Implies,
          BinCon(BinOp.Or, Atom("a"), UnCon(UnOp.Not, Atom("b"))),
          BinCon(BinOp.Iff, BinCon(BinOp.And, Atom("a"), Atom("d")), Atom("a"))
        ),
        BinCon(
          BinOp.Or,
          Const(Constant.Tautology),
          Const(Constant.Contradiction)
        )
      )
      assert(formula.equals(formulaParsed))
    }
    it("a generic Formula (Defeasible)") {
      val formulaParsed =
        Parser.parseFormula("(a=>b)~>((b||!c)&&d)")
      val formula: Formula = DefeasibleFormula(
        BinCon(BinOp.Implies, Atom("a"), Atom("b")),
        BinCon(
          BinOp.And,
          BinCon(BinOp.Or, Atom("b"), UnCon(UnOp.Not, Atom("c"))),
          Atom("d")
        )
      )
      assert(formula.equals(formulaParsed))
    }
    it("a string of DefeasibleFormulas and ClassicalFormulas") {
      val formulasParsed = Parser.parseStrings("a~>b,a=>b")
      val formulaSet = Set[Formula](
        DefeasibleFormula(Atom("a"), Atom("b")),
        BinCon(BinOp.Implies, Atom("a"), Atom("b"))
      )
      assert(formulaSet.equals(formulasParsed))
    }
  }
}
