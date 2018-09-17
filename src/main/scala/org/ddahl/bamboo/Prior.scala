package org.ddahl.bamboo

abstract class Prior {

  // log of unnormalized prior
  def apply(state: Bamboo): Double

}

