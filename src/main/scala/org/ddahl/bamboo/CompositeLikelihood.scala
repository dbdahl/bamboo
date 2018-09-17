package org.ddahl.bamboo

class CompositeLikelihood private (val weight: Double, val blockLikelihood1: BlockLikelihood, val blockLikelihood2: BlockLikelihood) extends BlockLikelihood {

  if ( weight < 0.0 || weight > 1.0 ) throw new IllegalArgumentException("Weight must be in [0,1].")

  def apply(antecedent: AminoAcidSequence, segment: AminoAcidSequence, descendant: AminoAcidSequence) = math.log( weight*math.exp(blockLikelihood1(antecedent,segment,descendant)) + (1-weight)*math.exp(blockLikelihood2(antecedent,segment,descendant)) )

  def reverse: CompositeLikelihood = this

  def countsDumpEngine(output: java.io.PrintWriter): Unit = {
    throw new RuntimeException("Should not be called.")
  }

  override def countsDump(directory: String, filename: String): Unit = {
    blockLikelihood1.countsDump(directory,filename)
    blockLikelihood2.countsDump(directory,filename+"_r")
  }

}

object CompositeLikelihood {

  def apply(weight: Double, blockLikelihood1: BlockLikelihood, blockLikelihood2: BlockLikelihood) = {
    new CompositeLikelihood(weight,blockLikelihood1,blockLikelihood2)
  }

}

