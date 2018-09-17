package org.ddahl.bamboo

class Conditional3PMF private (pmf: Array[Array[Array[ProbabilityMassFunction]]], indexPairs: Array[Tuple2[String,Int]]) {

  def apply(i: Int, j: Int, k: Int) = pmf(i)(j)(k)

  def countsDump(output: java.io.PrintWriter, label: String, includeZeros: Boolean = true): Unit = {
    indexPairs.foreach(x3 => {
      indexPairs.foreach(x2 => {
        indexPairs.foreach(x1 => pmf(x3._2)(x2._2)(x1._2).countsDump(output,label+x3._1+x2._1+x1._1,includeZeros))
      })
    })
  }

}

object Conditional3PMF extends Checks {

  def apply(pmf: Array[Array[Array[ProbabilityMassFunction]]], primary: Boolean) = {
    val indexPairs = if ( primary ) AAIndexPairs else SSIndexPairs
    check3(pmf,indexPairs.size)
    new Conditional3PMF(pmf.clone,indexPairs)
  }

}

