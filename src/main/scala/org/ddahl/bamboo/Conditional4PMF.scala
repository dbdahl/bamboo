package org.ddahl.bamboo

class Conditional4PMF private (pmf: Array[Array[Array[Array[ProbabilityMassFunction]]]], indexPairs: Array[Tuple2[String,Int]]) {

  def apply(i: Int, j: Int, k: Int, l: Int) = pmf(i)(j)(k)(l)

  def countsDump(output: java.io.PrintWriter, label: String, includeZeros: Boolean = true): Unit = {
    indexPairs.foreach(x4 => {
      indexPairs.foreach(x3 => {
        indexPairs.foreach(x2 => {
          indexPairs.foreach(x1 => pmf(x4._2)(x3._2)(x2._2)(x1._2).countsDump(output,label+x4._1+x3._1+x2._1+x1._1,includeZeros))
        })
      })
    })
  }

}

object Conditional4PMF extends Checks {

  def apply(pmf: Array[Array[Array[Array[ProbabilityMassFunction]]]], primary: Boolean) = {
    val indexPairs = if ( primary ) AAIndexPairs else SSIndexPairs
    check4(pmf,indexPairs.size)
    new Conditional4PMF(pmf.clone,indexPairs)
  }

}

