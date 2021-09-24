package kbgt.test

import org.scalatest.funspec.AnyFunSpec
import kbgt.logic.Parser
import org.tweetyproject.logics.pl.syntax._
import kbgt.logic._
import kbgt.generation._
import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

class GenerationSpec extends AnyFunSpec {
  describe("Conservative Defeasible Only Generation Tests") {
    it("Uniform Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .ConservativeRankedGenerate(
          r,
          s,
          DistributionFunction.Uniform,
          true
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Uniform(s, r, j)
        assert(rank.size == math.max(states, 2))
        j += 1
      }
    }
    it("Exponential Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .ConservativeRankedGenerate(
          r,
          s,
          DistributionFunction.Exponential,
          true
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Exponential(s, r, j)
        assert(rank.size == math.max(states, 2))
        j += 1
      }
    }
    it("Normal Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .ConservativeRankedGenerate(
          r,
          s,
          DistributionFunction.Normal,
          true
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Normal(s, r, j)
        assert(rank.size == math.max(states, 2))
        j += 1
      }
    }
    it("Inverted-Exponential Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .ConservativeRankedGenerate(
          r,
          s,
          DistributionFunction.InvertedExponential,
          true
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.InvertedExponential(s, r, j)
        assert(rank.size == math.max(states, 2))
        j += 1
      }
    }
    it("Inverted-Normal Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .ConservativeRankedGenerate(
          r,
          s,
          DistributionFunction.InvertedNormal,
          true
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.InvertedNormal(s, r, j)
        assert(rank.size == math.max(states, 2))
        j += 1
      }
    }
  }
  describe("Defeasible Only Generation Tests") {
    it("Uniform Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .RankedGenerate(
          r,
          s,
          DistributionFunction.Uniform,
          true
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Uniform(s, r, j)
        assert(rank.size == math.max(states, 3))
        j += 1
      }
    }
    it("Exponential Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .RankedGenerate(
          r,
          s,
          DistributionFunction.Exponential,
          true
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Exponential(s, r, j)
        assert(rank.size == math.max(states, 3))
        j += 1
      }
    }
    it("Normal Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .RankedGenerate(
          r,
          s,
          DistributionFunction.Normal,
          true
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Normal(s, r, j)
        assert(rank.size == math.max(states, 3))
        j += 1
      }
    }
    it("Inverted-Exponential Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .RankedGenerate(
          r,
          s,
          DistributionFunction.InvertedExponential,
          true
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.InvertedExponential(s, r, j)
        assert(rank.size == math.max(states, 3))
        j += 1
      }
    }
    it("Inverted-Normal Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .RankedGenerate(
          r,
          s,
          DistributionFunction.InvertedNormal,
          true
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.InvertedNormal(s, r, j)
        assert(rank.size == math.max(states, 3))
        j += 1
      }
    }
  }
  describe("Conservative Generation Tests") {
    it("Uniform Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .ConservativeRankedGenerate(
          r,
          s,
          DistributionFunction.Uniform,
          false
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Uniform(s, r, j)
        assert(rank.size == math.max(states, 1))
        j += 1
      }
    }
    it("Exponential Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .ConservativeRankedGenerate(
          r,
          s,
          DistributionFunction.Exponential,
          false
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Exponential(s, r, j)
        assert(rank.size == math.max(states, 1))
        j += 1
      }
    }
    it("Normal Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .ConservativeRankedGenerate(
          r,
          s,
          DistributionFunction.Normal,
          false
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Normal(s, r, j)
        assert(rank.size == math.max(states, 1))
        j += 1
      }
    }
    it("Inverted-Exponential Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .ConservativeRankedGenerate(
          r,
          s,
          DistributionFunction.InvertedExponential,
          false
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.InvertedExponential(s, r, j)
        assert(rank.size == math.max(states, 1))
        j += 1
      }
    }
    it("Inverted-Normal Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .ConservativeRankedGenerate(
          r,
          s,
          DistributionFunction.InvertedNormal,
          false
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.InvertedNormal(s, r, j)
        assert(rank.size == math.max(states, 1))
        j += 1
      }
    }
  }
  describe("Generation Tests") {
    it("Uniform Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .RankedGenerate(
          r,
          s,
          DistributionFunction.Uniform,
          false
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Uniform(s, r, j)
        assert(rank.size == math.max(states, 2))
        j += 1
      }
    }
    it("Exponential Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .RankedGenerate(
          r,
          s,
          DistributionFunction.Exponential,
          false
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Exponential(s, r, j)
        assert(rank.size == math.max(states, 2))
        j += 1
      }
    }
    it("Normal Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .RankedGenerate(
          r,
          s,
          DistributionFunction.Normal,
          false
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.Normal(s, r, j)
        assert(rank.size == math.max(states, 2))
        j += 1
      }
    }
    it("Inverted-Exponential Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .RankedGenerate(
          r,
          s,
          DistributionFunction.InvertedExponential,
          false
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.InvertedExponential(s, r, j)
        assert(rank.size == math.max(states, 2))
        j += 1
      }
    }
    it("Inverted-Normal Distribution") {
      val s = 50
      val r = 5
      var rankedKB = KBGenerator
        .RankedGenerate(
          r,
          s,
          DistributionFunction.InvertedNormal,
          false
        )
        .baseRank()
      assert(rankedKB.rankCount() == r)
      var j = 1
      for (rank <- rankedKB.defeasibleRanks()) {
        val states = DistributionFunction.InvertedNormal(s, r, j)
        assert(rank.size == math.max(states, 2))
        j += 1
      }
    }
  }

}
