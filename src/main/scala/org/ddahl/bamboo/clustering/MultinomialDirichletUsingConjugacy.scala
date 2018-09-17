//package org.ddahl.bamboo.clustering

//import org.ddahl.onion.r.jvm.ConjugateFunctions
//import org.ddahl.onion.util.RandomData
//import org.apache.commons.math3.special.Gamma.{logGamma => lgamma} 

/*
class MultinomialDirichletUsingConjugacy private (val y : Array[Array[Int]], val alpha : Array[Double], val seed : Long) extends ConjugateFunctions with Common {

  def newSufficientStatistic : Array[Double] = alpha.clone

  def add(index : Int, sufficientStatistic : Array[Double]) : Unit = {
    var j = 0
    val yy = y(index)
    while ( j < nFeatures ) {
      sufficientStatistic(j) += yy(j)
      j += 1
    }
  }

  def remove(index : Int, sufficientStatistic : Array[Double]) : Unit = {
    var j = 0
    val yy = y(index)
    while ( j < nFeatures ) {
      sufficientStatistic(j) -= yy(j)
      j += 1
    }
  }

  // log of ascending factorial function
  private def laff(base : Double, exponent : Double) = lgamma(base+exponent) - lgamma(base)

  def logMarginalLikelihood(index : Int, sufficientStatistic : Array[Double], size : Int) : Double = {
    val logNormalizingConstant = lFactorialN(index) - laff(sufficientStatistic.sum,n(index))
    val logKernel1 = sufficientStatistic.zip(y(index)).map(x => laff(x._1,x._2)).sum
    val logKernel2 = y(index).map(x => lgamma(x+1)).sum
    logNormalizingConstant + logKernel1 - logKernel2
  }

  def sampleParameterPrior() = random.nextDirichlet(alpha)

  def sampleParameterPosterior(sufficientStatistic: Array[Double], size: Int) = random.nextDirichlet(sufficientStatistic)

}

object MultinomialDirichletUsingConjugacy {

  def apply(y : Array[Array[Int]], alpha : Array[Double], seed : Long) = new MultinomialDirichletUsingConjugacy(y,alpha,seed)
  
}
*/

