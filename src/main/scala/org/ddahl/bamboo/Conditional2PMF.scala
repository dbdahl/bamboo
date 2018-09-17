package org.ddahl.bamboo

class Conditional2PMF private (pmf: Array[Array[ProbabilityMassFunction]], indexPairs: Array[Tuple2[String,Int]]) {

  def apply(i: Int, j: Int) = pmf(i)(j)

  def countsDump(output: java.io.PrintWriter, label: String, includeZeros: Boolean = true): Unit = {
    indexPairs.foreach(x2 => {
      indexPairs.foreach(x1 => pmf(x2._2)(x1._2).countsDump(output,label+x2._1+x1._1,includeZeros))
    })
  }

}

object Conditional2PMF extends Checks {

  def apply(pmf: Array[Array[ProbabilityMassFunction]], primary: Boolean) = {
    val indexPairs = if ( primary ) AAIndexPairs else SSIndexPairs
    check2(pmf,indexPairs.size)
    new Conditional2PMF(pmf.clone,indexPairs)
  }

}

