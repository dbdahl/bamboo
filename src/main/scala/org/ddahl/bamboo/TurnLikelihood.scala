package org.ddahl.bamboo

class TurnLikelihood private (
    val p31: ProbabilityMassFunction,
    val p32: Conditional1PMF,
    val p33: Conditional2PMF,
    val p41: ProbabilityMassFunction,
    val p42: Conditional1PMF,
    val p43: Conditional2PMF,
    val p51: ProbabilityMassFunction,
    val p52: Conditional1PMF,
    val p53: Conditional2PMF,
    val p54: Conditional2PMF,
    val p55: Conditional3PMF,
    forward: Boolean
  ) extends BlockLikelihood {

  def apply(antecedent: AminoAcidSequence, segment: AminoAcidSequence, descendant: AminoAcidSequence): Double = {
    val (a,t,d) = if ( forward ) (antecedent,segment,descendant) else (descendant.reverse,segment.reverse,antecedent.reverse)
    val k = segment.nPositions
    k match {
      case 3 =>
        p31(t(0)) + p32(t(0))(t(2)) + p33(t(0),t(2))(t(1))
      case 4 =>
        p41(t(0)) + p42(t(0))(t(3)) + p43(t(0),t(3))(t(1)) + p43(t(0),t(3))(t(2))
      case x if x > 4 =>
        var sum = p51(t(0)) + p52(t(0))(t(4)) + p53(t(0),t(4))(t(1)) + p53(t(0),t(4))(t(3)) + p54(t(1),t(3))(t(2))
        var i = 5
        while ( i < k ) {
          sum += p55(t(i-3),t(i-2),t(i-1))(t(i))
          i += 1
        }
        sum
    }
  }

  def reverse: TurnLikelihood = {
    new TurnLikelihood(p31,p32,p33,p41,p42,p43,p51,p52,p53,p54,p55,!forward)
  }

  def countsDumpEngine(output: java.io.PrintWriter): Unit = {
    p31.countsDump(output,"31")
    p32.countsDump(output,"32")
    p33.countsDump(output,"33")
    p41.countsDump(output,"41")
    p42.countsDump(output,"42")
    p43.countsDump(output,"43")
    p51.countsDump(output,"51")
    p52.countsDump(output,"52")
    p53.countsDump(output,"53")
    p54.countsDump(output,"54")
    p55.countsDump(output,"55")
  }

}

object TurnLikelihood extends BlockLikelihoodObject {

  def apply(
    p31: ProbabilityMassFunction,
    p32: Conditional1PMF,
    p33: Conditional2PMF,
    p41: ProbabilityMassFunction,
    p42: Conditional1PMF,
    p43: Conditional2PMF,
    p51: ProbabilityMassFunction,
    p52: Conditional1PMF,
    p53: Conditional2PMF,
    p54: Conditional2PMF,
    p55: Conditional3PMF
  ): TurnLikelihood = {
    new TurnLikelihood(p31,p32,p33,p41,p42,p43,p51,p52,p53,p54,p55,true)
  }

  def apply(bag: BagOfPMFs): TurnLikelihood = {
    val p31 = bag._0("31")
    val p32 = bag._1("32")
    val p33 = bag._2("33")
    val p41 = bag._0("41")
    val p42 = bag._1("42")
    val p43 = bag._2("43")
    val p51 = bag._0("51")
    val p52 = bag._1("52")
    val p53 = bag._2("53")
    val p54 = bag._2("54")
    val p55 = bag._3("55")
    apply(p31,p32,p33,p41,p42,p43,p51,p52,p53,p54,p55)
  }

}

