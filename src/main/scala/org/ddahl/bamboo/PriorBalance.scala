package org.ddahl.bamboo

class PriorBalance private () extends Prior {

  private val logH = math.log(0.3590)
  private val logE = math.log(0.2078)
  private val logC = math.log(0.4332)

  def apply(state: Bamboo): Double = {
    (state.ssBlocks :+ (" ",0)).zip((" ",0) +: state.ssBlocks).foreach { case (x,y) =>
      if ( ( x._1 == "H" ) && ( y._1 == "E" ) ) return Double.NegativeInfinity
      if ( ( x._1 == "E" ) && ( y._1 == "H" ) ) return Double.NegativeInfinity
    }
    val sequence = state.asSequence
    sequence.map( _ match {
      case "H" => logH
      case "E" => logE
      case "C" => logC
    }).sum
  }

}

object PriorBalance {

  def apply(): PriorBalance = new PriorBalance()

}

