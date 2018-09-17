package org.ddahl.bamboo

class HelixLikelihood private (
    val p1: ProbabilityMassFunction,
    val p2: Conditional1PMF,
    val p3: Conditional2PMF,
    val p4: Conditional2PMF,
    val p5: Conditional3PMF,
    forward: Boolean
  ) extends BlockLikelihood {

  def apply(antecedent: AminoAcidSequence, segment: AminoAcidSequence, descendant: AminoAcidSequence): Double = {
    val (a,t,d) = if ( forward ) (antecedent,segment,descendant) else (descendant.reverse,segment.reverse,antecedent.reverse)
    val k = segment.nPositions
    var sum = p1(t(0)) + p2(t(0))(t(1)) + p3(t(0),t(1))(t(2))
    if ( k > 3 ) {
      sum += p4(t(1),t(2))(t(3))
      var i = 4
      while ( i < k ) {
        sum += p5(t(i-4),t(i-3),t(i-1))(t(i))
        i += 1
      }
    }
    sum
  }

  def reverse: HelixLikelihood = {
    new HelixLikelihood(p1,p2,p3,p4,p5,!forward)
  }

  def countsDumpEngine(output: java.io.PrintWriter): Unit = {
    p1.countsDump(output,"1")
    p2.countsDump(output,"2")
    p3.countsDump(output,"3")
    p4.countsDump(output,"4")
    p5.countsDump(output,"5")
  }

}

object HelixLikelihood extends BlockLikelihoodObject {

  def apply(p1: ProbabilityMassFunction, p2: Conditional1PMF, p3: Conditional2PMF, p4: Conditional2PMF, p5: Conditional3PMF): HelixLikelihood = {
    new HelixLikelihood(p1,p2,p3,p4,p5,true)
  }

  def apply(bag: BagOfPMFs): HelixLikelihood = {
    val p1 = bag._0("1")
    val p2 = bag._1("2")
    val p3 = bag._2("3")
    val p4 = bag._2("4")
    val p5 = bag._3("5")
    apply(p1,p2,p3,p4,p5)
  }

}

