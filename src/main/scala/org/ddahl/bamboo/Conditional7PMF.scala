package org.ddahl.bamboo

class Conditional7PMF private (pmf: Array[Array[Array[Array[Array[Array[Array[ProbabilityMassFunction]]]]]]], indexPairs: Array[Tuple2[String,Int]]) {

  def apply(i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int) = pmf(i)(j)(k)(l)(m)(n)(o)

  def countsDump(output: java.io.PrintWriter, label: String, includeZeros: Boolean = true): Unit = {
    indexPairs.foreach(x7 => {
      indexPairs.foreach(x6 => {
        indexPairs.foreach(x5 => {
          indexPairs.foreach(x4 => {
            indexPairs.foreach(x3 => {
              indexPairs.foreach(x2 => {
                indexPairs.foreach(x1 => pmf(x7._2)(x6._2)(x5._2)(x4._2)(x3._2)(x2._2)(x1._2).countsDump(output,label+x7._1+x6._1+x5._1+x4._1+x3._1+x2._1+x1._1,includeZeros))
              })
            })
          })
        })
      })
    })
  }

}

object Conditional7PMF extends Checks {

  def apply(pmf: Array[Array[Array[Array[Array[Array[Array[ProbabilityMassFunction]]]]]]], primary: Boolean) = {
    val indexPairs = if ( primary ) AAIndexPairs else SSIndexPairs
    check7(pmf,indexPairs.size)
    new Conditional7PMF(pmf.clone,indexPairs)
  }

}

