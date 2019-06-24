package org.ddahl.bamboo

class PriorDBD private (val center: SSBlocks, val reliability: List[Double], val alpha: Double) extends Prior {

  if ( center.map(_._2).sum != reliability.size ) throw new IllegalArgumentException("Inconsistent number of positions.")

  private val seq = Bamboo.toSequence(center)

  def apply(state: Bamboo): Double = {
    (state.ssBlocks :+ (" ",0)).zip((" ",0) +: state.ssBlocks).foreach { case (x,y) =>
      if ( ( x._1 == "H" ) && ( y._1 == "E" ) ) return Double.NegativeInfinity
      if ( ( x._1 == "E" ) && ( y._1 == "H" ) ) return Double.NegativeInfinity
    }
    val equal = state.asSequence.zip(seq).map { case(x,y) => x == y }
    alpha * equal.zip(reliability).map { case (x, r) =>
      if (x) r else 0
    }.sum
  }

}

object PriorDBD {

  def apply(center: SSBlocks, reliability: List[Double], alpha: Double): PriorDBD = new PriorDBD(center,reliability,alpha)

}

