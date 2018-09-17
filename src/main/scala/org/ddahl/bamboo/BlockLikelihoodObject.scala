package org.ddahl.bamboo

abstract class BlockLikelihoodObject {

  def apply(bag: BagOfPMFs): BlockLikelihood

  def apply(): BlockLikelihood = {
    apply(BagOfPMFs(true))
  }

  def apply(filename: String): BlockLikelihood = {
    if ( ( filename == null ) || ( filename == "" ) || ( filename == "." ) ) apply()
    else apply(BagOfPMFs(filename,true))
  }

}

