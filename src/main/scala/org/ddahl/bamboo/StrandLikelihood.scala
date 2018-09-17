package org.ddahl.bamboo

class StrandLikelihood private (
    val p1o: ProbabilityMassFunction,
    val p1e: ProbabilityMassFunction,
    val p2o: Conditional1PMF,
    val p2e: Conditional1PMF,
    val p3o: Conditional2PMF,
    val p3e: Conditional2PMF,
    forward: Boolean
  ) extends BlockLikelihood {

  def apply(antecedent: AminoAcidSequence, segment: AminoAcidSequence, descendant: AminoAcidSequence): Double = {
    val (a,t,d) = if ( forward ) (antecedent,segment,descendant) else (descendant.reverse,segment.reverse,antecedent.reverse)
    val k = segment.nPositions
    var sum = p1o(t(0)) + p1e(t(1)) + p2o(t(0))(t(2))
    if ( k > 3 ) {
      sum += p2e(t(1))(t(3))
      var i = 4
      while ( i < k ) {
        sum += ( if ( i % 2 == 0 ) p3o(t(i-4),t(i-2))(t(i)) else p3e(t(i-4),t(i-2))(t(i)) )
        i += 1
      }
    }
    sum
  }

  def reverse: StrandLikelihood = {
    new StrandLikelihood(p1o,p1e,p2o,p2e,p3o,p3e,!forward)
  }

  def countsDumpEngine(output: java.io.PrintWriter): Unit = {
    p1o.countsDump(output,"1")
    p1e.countsDump(output,"2")
    p2o.countsDump(output,"3")
    p2e.countsDump(output,"4")
    p3o.countsDump(output,"5")
    p3e.countsDump(output,"6")
  }

}

object StrandLikelihood extends BlockLikelihoodObject {

  def apply(p1o: ProbabilityMassFunction, p1e: ProbabilityMassFunction, p2o: Conditional1PMF, p2e: Conditional1PMF, p3o: Conditional2PMF, p3e: Conditional2PMF): StrandLikelihood = {
    new StrandLikelihood(p1o,p1e,p2o,p2e,p3o,p3e,true)
  }

  def apply(bag: BagOfPMFs): StrandLikelihood = {
    val p1o = bag._0("1")
    val p1e = bag._0("2")
    val p2o = bag._1("3")
    val p2e = bag._1("4")
    val p3o = bag._2("5")
    val p3e = bag._2("6")
    apply(p1o,p1e,p2o,p2e,p3o,p3e)
  }

}

