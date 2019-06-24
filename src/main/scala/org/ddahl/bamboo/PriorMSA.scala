package org.ddahl.bamboo

class PriorMSA private (
    val p: Array[ProbabilityMassFunction]
  ) extends Prior {

  def apply(state: Bamboo): Double = {
    (state.ssBlocks :+ (" ",0)).zip((" ",0) +: state.ssBlocks).foreach { case (x,y) =>
      if ( ( x._1 == "H" ) && ( y._1 == "E" ) ) return Double.NegativeInfinity
      if ( ( x._1 == "E" ) && ( y._1 == "H" ) ) return Double.NegativeInfinity
    }
    val t = state.asSequence.map(x => SSIndexMap(x.toString)).toArray
    if ( t.length != p.length ) throw new IllegalArgumentException()
    var sum = 0.0
    for ( i <- 0 until p.length ) {
      sum += p(i)(t(i))
    }
    sum
  }

}

object PriorMSA {

  def apply(p: Array[ProbabilityMassFunction]): PriorMSA = new PriorMSA(p)

  def apply(counts: Array[Array[Int]], alpha: Array[Double]): PriorMSA = {
    val weights = counts.map(_.zip(alpha).map(x => x._1 + x._2))
    val totals = weights.map(_.sum)
    val probabilities = weights.zip(totals).map(x => x._1.map(_/x._2))
    val p = probabilities.map(x => ProbabilityMassFunction(x,false))
    new PriorMSA(p)
  }

}

