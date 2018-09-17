package org.ddahl.bamboo

class AminoAcidSequence private (seq: Array[Int]) {

  def apply(i: Int) = seq(i)

  def splitAt(n: Int) = {
    val x = seq.splitAt(n)
    (new AminoAcidSequence(x._1),new AminoAcidSequence(x._2))
  }

  def sliceWithEnds(start: Int, stop: Int) = {
    val antecedent = seq.slice(math.max(start-3,0),start).reverse
    val current    = seq.slice(start,stop)
    val descendant = seq.slice(stop,math.min(stop+3,seq.length))
    (new AminoAcidSequence(antecedent),new AminoAcidSequence(current),new AminoAcidSequence(descendant))
  }

  def reverse = new AminoAcidSequence(seq.reverse)

  val nPositions = seq.length

}

object AminoAcidSequence {

  def apply(sequence: String) = {
    new AminoAcidSequence(sequence.map(x => AAIndexMap(x.toString)).toArray)
  }

}

