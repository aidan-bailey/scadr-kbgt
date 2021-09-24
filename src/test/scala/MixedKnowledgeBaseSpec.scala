package kbgt.test

import org.scalatest.funspec.AnyFunSpec
import kbgt.logic.Parser
import org.tweetyproject.logics.pl.syntax._
import kbgt.logic._
import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

class MixedKnowledgeBaseSpec extends AnyFunSpec {
  describe("The MixedKnowledgeBase can") {
    it("be constructed from a sequence of MixedKnowledgeBase") {
      val kb = new MixedKnowledgeBase(
        Atom("a"),
        DefeasibleFormula(Atom("a"), Atom("b"))
      )
      assert(
        kb.contains(DefeasibleFormula(Atom("a"), Atom("b"))) && kb.contains(
          Atom("a")
        )
      )
    }
    it("have Formulas added to it") {
      val kb = new MixedKnowledgeBase()
      kb.add(DefeasibleFormula(Atom("a"), Atom("b")))
      kb.add(Atom("a"))
      kb.addAll(
        Seq(
          DefeasibleFormula(Atom("b"), Atom("c")),
          Atom("b")
        )
      )
      assert(
        kb.contains(DefeasibleFormula(Atom("a"), Atom("b"))) && kb.contains(
          Atom("a")
        ) && kb.contains(
          DefeasibleFormula(Atom("b"), Atom("c"))
        ) && kb.contains(Atom("b"))
      )
    }
    it("have Formulas removed from it") {
      val kb = new MixedKnowledgeBase(
        DefeasibleFormula(Atom("a"), Atom("b")),
        Atom("c")
      )
      kb.remove(DefeasibleFormula(Atom("a"), Atom("b")))
      kb.remove(Atom("c"))
      assert(
        !kb.contains(DefeasibleFormula(Atom("a"), Atom("b"))) && !kb.contains(
          Atom("c")
        )
      )
    }
    it("not contain duplicates of Formulas") {
      val kb = new MixedKnowledgeBase(
        DefeasibleFormula(Atom("a"), Atom("b")),
        DefeasibleFormula(Atom("a"), Atom("b")),
        Atom("a"),
        Atom("a")
      )
      kb.add(DefeasibleFormula(Atom("a"), Atom("b")))
      kb.add(Atom("a"))
      kb.addAll(
        Seq(
          DefeasibleFormula(Atom("a"), Atom("b")),
          DefeasibleFormula(Atom("a"), Atom("b")),
          Atom("a"),
          Atom("a")
        )
      )
      assert(kb.size == 2)
    }
    it("be combined with other MixedKnowledgeBases") {
      val kb1 =
        new MixedKnowledgeBase(
          DefeasibleFormula(Atom("a"), Atom("b")),
          Atom("a")
        )
      val kb2 = new MixedKnowledgeBase(
        DefeasibleFormula(Atom("b"), Atom("a")),
        Atom("b")
      )
      val kb = new MixedKnowledgeBase()
      kb.addAll(kb1)
      kb.addAll(kb2)
      assert(
        kb.contains(DefeasibleFormula(Atom("a"), Atom("b"))) && kb.contains(
          Atom("a")
        ) && kb.contains(DefeasibleFormula(Atom("b"), Atom("a"))) && kb
          .contains(Atom("b"))
      )
    }
    it("be written to and loaded from json file") {
      val kb =
        new MixedKnowledgeBase(DefeasibleFormula(Atom("a"), Atom("b")))
      kb.writeFile("test.json")
      val kb2 = new MixedKnowledgeBase().loadFile("test.json")
      new File("test.json").delete()
      assert(kb.equals(kb2))
    }
    it("be loaded from an json file containing an empty json array") {
      val file = new File("test.json")
      file.createNewFile()
      val pw = new PrintWriter(file)
      pw.write("[]")
      pw.close()
      val kb = new MixedKnowledgeBase().loadFile("test.json")
      file.delete()
      assert(kb.equals(new DefeasibleKnowledgeBase))
    }
    it("be cleared") {
      val kb = new MixedKnowledgeBase(
        DefeasibleFormula(Atom("a"), Atom("b")),
        Atom("a")
      )
      kb.clear()
      assert(
        kb.size == 0
      )
    }
    it("be cloned") {
      val kb = new MixedKnowledgeBase(
        DefeasibleFormula(Atom("a"), Atom("b")),
        Atom("a")
      )
      val kbClone = kb.clone()
      assert(
        kbClone.contains(DefeasibleFormula(Atom("a"), Atom("b"))) && kbClone
          .contains(Atom("a"))
      )
      kbClone.clear()
      assert(kb.nonEmpty)
    }
    it("produce a correct base rank") {
      val kb = new MixedKnowledgeBase(
        DefeasibleFormula(Atom("b"), Atom("f")),
        DefeasibleFormula(Atom("b"), Atom("w")),
        DefeasibleFormula(Atom("p"), UnCon(UnOp.Not, Atom("f"))),
        BinCon(BinOp.Implies, Atom("p"), Atom("b")),
        BinCon(BinOp.Implies, Atom("t"), Atom("p"))
      )
      val defeasibleRanks = ListBuffer[MixedKnowledgeBase](
        new MixedKnowledgeBase(
          DefeasibleFormula(Atom("b"), Atom("f")),
          DefeasibleFormula(Atom("b"), Atom("w"))
        ),
        new MixedKnowledgeBase(
          DefeasibleFormula(Atom("p"), UnCon(UnOp.Not, Atom("f")))
        )
      )
      val infiniteRank = new MixedKnowledgeBase(
        BinCon(BinOp.Implies, Atom("p"), Atom("b")),
        BinCon(BinOp.Implies, Atom("t"), Atom("p"))
      )
      val rankedKB = kb.baseRank()
      assert(
        rankedKB.defeasibleRanks.equals(defeasibleRanks) && rankedKB
          .infiniteRank()
          .equals(infiniteRank)
      )
    }
  }

}
