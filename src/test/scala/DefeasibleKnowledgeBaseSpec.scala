package kbgt.test

import org.scalatest.funspec.AnyFunSpec
import kbgt.logic.Parser
import org.tweetyproject.logics.pl.syntax._
import kbgt.logic._
import java.io.File
import java.io.PrintWriter

class DefeasibleKnowledgeBaseSpec extends AnyFunSpec {
  describe("The DefeasibleKnowledgeBase can") {
    it("be constructed from a sequence of DefeasibleFormulas") {
      val dkb = new DefeasibleKnowledgeBase(
        DefeasibleFormula(Atom("a"), Atom("b")),
        DefeasibleFormula(Atom("a"), Atom("b"))
      )
      assert(
        dkb.contains(DefeasibleFormula(Atom("a"), Atom("b"))) && dkb.contains(
          DefeasibleFormula(Atom("a"), Atom("b"))
        )
      )
    }
    it("have DefeasibleFormulas added to it") {
      val dkb = new DefeasibleKnowledgeBase()
      dkb.add(DefeasibleFormula(Atom("a"), Atom("b")))
      dkb.addAll(
        Seq(
          DefeasibleFormula(Atom("b"), Atom("c")),
          DefeasibleFormula(Atom("c"), Atom("d"))
        )
      )
      assert(
        dkb.contains(DefeasibleFormula(Atom("a"), Atom("b"))) && dkb.contains(
          DefeasibleFormula(Atom("b"), Atom("c"))
        ) && dkb.contains(
          DefeasibleFormula(Atom("c"), Atom("d"))
        )
      )
    }
    it("have DefeasibleFormulas removed from it") {
      val dkb = new DefeasibleKnowledgeBase(
        DefeasibleFormula(Atom("a"), Atom("b")),
        DefeasibleFormula(Atom("b"), Atom("c"))
      )
      dkb.remove(DefeasibleFormula(Atom("a"), Atom("b")))
      assert(!dkb.contains(DefeasibleFormula(Atom("a"), Atom("b"))))
    }
    it("not contain duplicates of formulas") {
      val dkb = new DefeasibleKnowledgeBase(
        DefeasibleFormula(Atom("a"), Atom("b")),
        DefeasibleFormula(Atom("a"), Atom("b"))
      )
      dkb.add(DefeasibleFormula(Atom("a"), Atom("b")))
      dkb.addAll(
        Seq(
          DefeasibleFormula(Atom("a"), Atom("b")),
          DefeasibleFormula(Atom("a"), Atom("b"))
        )
      )
      assert(dkb.size == 1)
    }
    it("be combined with other DefeasibleKnowledgeBases") {
      val dkb1 =
        new DefeasibleKnowledgeBase(
          DefeasibleFormula(Atom("a"), Atom("b")),
          DefeasibleFormula(Atom("a"), Atom("c"))
        )
      val dkb2 = new DefeasibleKnowledgeBase(
        DefeasibleFormula(Atom("b"), Atom("a")),
        DefeasibleFormula(Atom("b"), Atom("c"))
      )
      val dkb = new DefeasibleKnowledgeBase()
      dkb.addAll(dkb1)
      dkb.addAll(dkb2)
      assert(
        dkb.contains(DefeasibleFormula(Atom("a"), Atom("b"))) && dkb.contains(
          DefeasibleFormula(Atom("a"), Atom("c"))
        ) && dkb.contains(
          DefeasibleFormula(Atom("b"), Atom("a"))
        ) && dkb.contains(DefeasibleFormula(Atom("b"), Atom("c")))
      )
    }
    it("be written to and loaded from json file") {
      val dkb =
        new DefeasibleKnowledgeBase(DefeasibleFormula(Atom("a"), Atom("b")))
      dkb.writeFile("test.json")
      val dkb2 = new DefeasibleKnowledgeBase().loadFile("test.json")
      new File("test.json").delete()
      assert(dkb2.equals(dkb))
    }
    it("be loaded from an json file containing an empty json array") {
      val file = new File("test.json")
      file.createNewFile()
      val pw = new PrintWriter(file)
      pw.write("[]")
      pw.close()
      val dkb = new DefeasibleKnowledgeBase().loadFile("test.json")
      file.delete()
      assert(dkb.equals(new DefeasibleKnowledgeBase))
    }
    it("be cleared") {
      val ckb = new DefeasibleKnowledgeBase(
        DefeasibleFormula(Atom("a"), Atom("b")),
        DefeasibleFormula(Atom("b"), Atom("c"))
      )
      ckb.clear()
      assert(
        ckb.size == 0
      )
    }
    it("be cloned") {
      val ckb = new DefeasibleKnowledgeBase(
        DefeasibleFormula(Atom("a"), Atom("b")),
        DefeasibleFormula(Atom("b"), Atom("c"))
      )
      val ckbClone = ckb.clone()
      assert(
        ckbClone.contains(DefeasibleFormula(Atom("a"), Atom("b"))) && ckbClone
          .contains(DefeasibleFormula(Atom("b"), Atom("c")))
      )
      ckbClone.clear()
      assert(ckb.nonEmpty)
    }
  }

}
