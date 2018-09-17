package org.ddahl.bamboo

class CoilXLikelihood private (
    val p1: ProbabilityMassFunction,
    val p2: Conditional1PMF,
    val p3: Conditional2PMF,
    forward: Boolean
  ) extends BlockLikelihood {

  def apply(antecedent: AminoAcidSequence, segment: AminoAcidSequence, descendant: AminoAcidSequence): Double = {
    val (a,t,d) = if ( forward ) (antecedent,segment,descendant) else (descendant.reverse,segment.reverse,antecedent.reverse)
    val k = segment.nPositions
    var sum = p1(t(0))
    if ( k > 1 ) {
      sum += p2(t(0))(t(1))
      if ( k > 2 ) {
        var i = 2
        while ( i < k ) {
          sum += p3(t(i-2),t(i-1))(t(i))
          i += 1
        }
      }
    }
    sum
  }

  def reverse: CoilXLikelihood = {
    new CoilXLikelihood(p1,p2,p3,!forward)
  }

  def countsDumpEngine(output: java.io.PrintWriter): Unit = {
    p1.countsDump(output,"1")
    p2.countsDump(output,"2")
    p3.countsDump(output,"3")
  }

}

object CoilXLikelihood extends BlockLikelihoodObject {

  def apply(p1: ProbabilityMassFunction, p2: Conditional1PMF, p3: Conditional2PMF): CoilXLikelihood = {
    new CoilXLikelihood(p1,p2,p3,true)
  }

  def apply(bag: BagOfPMFs): CoilXLikelihood = {
    val p1 = bag._0("1")
    val p2 = bag._1("2")
    val p3 = bag._2("3")
    apply(p1,p2,p3)
  }

}

