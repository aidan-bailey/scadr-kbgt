package kbgt.test

import org.scalatest.funspec.AnyFunSpec
import kbgt.logic.Parser
import org.tweetyproject.logics.pl.syntax._
import kbgt.logic._
import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

class RankedKnowledgeBaseSpec extends AnyFunSpec {
  describe("The RankedKnowledgeBase can") {
    it("be written to and read from a json file") {
      val rkb = new RankedKnowledgeBase(
        ListBuffer[MixedKnowledgeBase](
          new MixedKnowledgeBase(DefeasibleFormula(Atom("a"), Atom("b")))
        ),
        new MixedKnowledgeBase(Atom("a"), Atom("b"))
      )
      rkb.writeFile("test.json")
      val rkb2: RankedKnowledgeBase =
        new RankedKnowledgeBase().readFile("test.json")
      new File("test.json").delete()
      assert(rkb2.equals(rkb))
    }
    it("be written to and read from an empty json array file") {
      val rkb = new RankedKnowledgeBase
      rkb.writeFile("test.json")
      rkb.readFile("test.json")
      new File("test.json").delete()
      assert(rkb.equals(new RankedKnowledgeBase))
    }
    it("can be replaced") {
      val rkb = new RankedKnowledgeBase(
        ListBuffer[MixedKnowledgeBase](
          new MixedKnowledgeBase(DefeasibleFormula(Atom("a"), Atom("b")))
        ),
        new MixedKnowledgeBase(Atom("a"), Atom("b"))
      )
      val rkb2 = new RankedKnowledgeBase(
        ListBuffer[MixedKnowledgeBase](
          new MixedKnowledgeBase(DefeasibleFormula(Atom("p"), Atom("q")))
        ),
        new MixedKnowledgeBase(Atom("z"), Atom("e"))
      )
      rkb.replace(rkb2)
      assert(rkb.equals(rkb2))
    }
    it("can be cleared") {
      val rkb = new RankedKnowledgeBase(
        ListBuffer[MixedKnowledgeBase](
          new MixedKnowledgeBase(DefeasibleFormula(Atom("a"), Atom("b")))
        ),
        new MixedKnowledgeBase(Atom("a"), Atom("b"))
      )
      rkb.clear()
      assert(rkb.defeasibleRanks().isEmpty && rkb.infiniteRank().isEmpty)
    }
  }
}
