package org.ddahl.bamboo

class PriorDBD private (val center: SSBlocks, val reliability: List[Double], val alpha: Double) extends Prior {

  if ( center.map(_._2).sum != reliability.size ) throw new IllegalArgumentException("Inconsistent number of positions.")

  private val seq = Bamboo.toSequence(center)

  def apply(state: Bamboo): Double = {
    (state.ssBlocks :+ (" ",0), (" ",0) +: state.ssBlocks).zipped.foreach( (x,y) => {
      if ( ( x._1 == "H" ) && ( y._1 == "E" ) ) return Double.NegativeInfinity
      if ( ( x._1 == "E" ) && ( y._1 == "H" ) ) return Double.NegativeInfinity
    })
    alpha * (state.asSequence,seq,reliability).zipped.map( (x,y,r) => if ( x == y ) r else 0 ).sum
  }

}

object PriorDBD {

  def apply(center: SSBlocks, reliability: List[Double], alpha: Double): PriorDBD = new PriorDBD(center,reliability,alpha)

}

