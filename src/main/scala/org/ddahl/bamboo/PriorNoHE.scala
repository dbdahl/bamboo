package org.ddahl.bamboo

class PriorNoHE private () extends Prior {

  def apply(state: Bamboo): Double = {
    (state.ssBlocks :+ (" ",0), (" ",0) +: state.ssBlocks).zipped.foreach( (x,y) => {
      if ( ( x._1 == "H" ) && ( y._1 == "E" ) ) return Double.NegativeInfinity
      if ( ( x._1 == "E" ) && ( y._1 == "H" ) ) return Double.NegativeInfinity
    })
    0.0
  }

}

object PriorNoHE {

  def apply(): PriorNoHE = new PriorNoHE()

}

