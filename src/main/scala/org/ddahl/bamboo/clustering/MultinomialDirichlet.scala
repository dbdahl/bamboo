//package org.ddahl.bamboo.clustering

//import org.ddahl.onion.r.jvm.NonconjugateFunctions
//import org.ddahl.onion.util.RandomData
//import org.apache.commons.math3.special.Gamma.{logGamma => lgamma} 
//import scala.math.log

/*
class MultinomialDirichlet private (val y : Array[Array[Int]], val alpha : Array[Double], val seed : Long) extends NonconjugateFunctions with Common {

  if ( alpha.length != nFeatures ) throw new IllegalArgumentException("Inconsistent number of features.")

  def logLikelihood(index : Int, parameter : Array[Double]) = logLikelihood(lFactorialN(index),y(index),parameter)

  private val logPriorNormalizingConstant = lgamma(alpha.sum) - alpha.map(lgamma).sum

  def logPrior(parameter : Array[Double]) : Double = {
    val logKernel = alpha.zip(parameter).map(x => (x._1-1)*log(x._2)).sum
    logKernel + logPriorNormalizingConstant
  }

  def samplePrior : Array[Double] = random.nextDirichlet(alpha)

  def update(current : Array[Double], indices : Array[Int]) : Array[Double] = {
    val beta = alpha.clone
    indices.foreach { i =>
      val yy = y(i)
      for ( j <- 0 until nFeatures ) {
        beta(j) += yy(j)
      }
    }
    random.nextDirichlet(beta)
  }

}

object MultinomialDirichlet {

  def apply(y : Array[Array[Int]], alpha : Array[Double], seed : Long) = new MultinomialDirichlet(y,alpha,seed)
 
}
*/

