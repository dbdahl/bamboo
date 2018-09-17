package org.ddahl.bamboo

class Conditional1PMF private (pmf: Array[ProbabilityMassFunction], indexPairs: Array[Tuple2[String,Int]]) {

  def apply(i: Int) = pmf(i)

  def countsDump(output: java.io.PrintWriter, label: String, includeZeros: Boolean = true): Unit = {
    indexPairs.foreach(x1 => pmf(x1._2).countsDump(output,label+x1._1,includeZeros))
  }

}

object Conditional1PMF extends Checks {

  def apply(pmf: Array[ProbabilityMassFunction], primary: Boolean) = {
    val indexPairs = if ( primary ) AAIndexPairs else SSIndexPairs
    check1(pmf,indexPairs.size)
    new Conditional1PMF(pmf.clone,indexPairs)
  }

}

