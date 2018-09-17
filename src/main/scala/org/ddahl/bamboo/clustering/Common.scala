//package org.ddahl.bamboo.clustering

//import org.ddahl.onion.Debug
//import org.ddahl.onion.util.RandomData
//import org.apache.commons.math3.special.Gamma.{logGamma => lgamma}
//import scala.math.log

/*
trait Common {

  val y : Array[Array[Int]]
  val alpha : Array[Double]
  val seed : Long

  val nItems = y.length

  if ( nItems == 0 ) throw new IllegalArgumentException("Number of items must be a least one.")

  val nFeatures = alpha.length

  for ( i <- 0 until nItems ) {
    if ( y(i).length != nFeatures ) throw new IllegalArgumentException("Inconsistent number of features.")
  }

  val n = y.map(_.sum)
  protected val lFactorialN = n.map(x => lgamma(x+1))

  val random = RandomData(seed)

  def impute(index: Int, parameter: Array[Double]) = {
    y(index) = random.nextMultinomial(n(index),parameter)
  }

  def data(index : Int) = y(index).map(_.toDouble)

  def copyOtherParameters(otherParameters : Array[Double]) : Unit = {}

  def logLikelihood(data : Array[Int], parameter : Array[Double]) : Double = {
    val lfn = lgamma(data.sum+1)
    logLikelihood(lfn,data,parameter)
  }

  def logLikelihood(lfn : Double, data : Array[Int], parameter : Array[Double]) : Double = {
    val logNormalizingConstant = data.map(x => lgamma(x+1)).sum - lfn
    val logKernel = data.zip(parameter).map(x => x._1*log(x._2)).sum
    logKernel - logNormalizingConstant
  }

}

object Common {

  def arrayToMatrix(array : Array[Int], nFeatures : Int) = {
    val nItems = array.length / nFeatures
    val matrix = new Array[Array[Int]](nItems)
    for ( i <- 0 until nItems ) {
      matrix(i) = new Array[Int](nFeatures)
      for ( j <- 0 until nFeatures ) {
        matrix(i)(j) = array(j*nItems+i)
      }
    }
    matrix
  }

}
*/

