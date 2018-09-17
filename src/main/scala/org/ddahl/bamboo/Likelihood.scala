package org.ddahl.bamboo

class Likelihood private (val blockLikelihoodMap: Map[String,BlockLikelihood], val aa: AminoAcidSequence) {

  def apply(state: Bamboo): Double = {
    var pos = 0
    state.ssBlocks.map( block => {
      val segments = aa.sliceWithEnds(pos,pos+block._2)
      pos += block._2
      blockLikelihoodMap(block._1)(segments._1,segments._2,segments._3)
    }).sum
  }

  def oneOff(aa: String, ss: String) = Likelihood(blockLikelihoodMap,AminoAcidSequence(aa))(Bamboo(ss))

  def count(aa: Array[String], ss: Array[String], directory: String): Unit = {
    aa.zip(ss).foreach( x => Likelihood(blockLikelihoodMap,AminoAcidSequence(x._1))(Bamboo(x._2)) )
    (new java.io.File(directory)).mkdirs
    blockLikelihoodMap.foreach( x => x._2.countsDump(directory,x._1) )
  }

}

object Likelihood {

  def apply(blockLikelihoodMap: Map[String,BlockLikelihood], aa: AminoAcidSequence): Likelihood = {
    new Likelihood(blockLikelihoodMap,aa)
  }

  def apply(likelihoodString: String, aa: AminoAcidSequence): Likelihood = {
    apply(parse(likelihoodString),aa)
  }

  def factory(blockLikelihoodMap: Map[String,BlockLikelihood]): (AminoAcidSequence) => Likelihood = {
    (aa: AminoAcidSequence) => {
      apply(blockLikelihoodMap,aa)
    }
  }

  def factory(likelihoodString: String): (AminoAcidSequence) => Likelihood = {
    (aa: AminoAcidSequence) => {
      apply(parse(likelihoodString),aa)
    }
  }

  private def parse(likelihoodString: String): Map[String,BlockLikelihood] = {
    val map = new scala.collection.mutable.HashMap[String,BlockLikelihood]()
    val cells = likelihoodString.split("""\s*;\s*""").map(_.split("""\s*:\s*""")).foreach( x => {
      if ( x(0) != "" ) {
        val likelihood = x(0) match {
          case "H" => if ( x.length == 2 ) HelixLikelihood(x(1))
                      else CompositeLikelihood(0.5,HelixLikelihood(x(1)),HelixLikelihood(x(2)).reverse)
          case "E" => if ( x.length == 2 ) StrandLikelihood(x(1))
                      else CompositeLikelihood(0.5,StrandLikelihood(x(1)),StrandLikelihood(x(2)).reverse)
          case "T" => if ( x.length == 2 ) TurnLikelihood(x(1))
                      else CompositeLikelihood(0.5,TurnLikelihood(x(1)),TurnLikelihood(x(2)).reverse)
          case "C" => if ( x.length == 2 ) CoilLikelihood(x(1))
                      else CompositeLikelihood(0.5,CoilLikelihood(x(1)),CoilLikelihood(x(2)).reverse)
          case "K" => if ( x.length == 2 ) CoilXLikelihood(x(1))
                      else CompositeLikelihood(0.5,CoilXLikelihood(x(1)),CoilXLikelihood(x(2)).reverse)
          case "O" => if ( x.length == 2 ) CoilXLikelihood(x(1))
                      else CompositeLikelihood(0.5,CoilXLikelihood(x(1)),CoilXLikelihood(x(2)).reverse)
          case "S" => if ( x.length == 2 ) CoilXLikelihood(x(1))
                      else CompositeLikelihood(0.5,CoilXLikelihood(x(1)),CoilXLikelihood(x(2)).reverse)
        }
        map(x(0)) = likelihood
      }
    })
    map.toMap
  }

}

