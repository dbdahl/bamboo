package org.ddahl.bamboo

class ProbabilityMassFunction private (logProbabilities: Array[Double]) {

  private val counts: Array[Int] = newCounts()
  private val rng = newRNG()
  val training = ( counts != null )

  def countsDump(output: java.io.PrintWriter, label: String, includeZeros: Boolean = true): Unit = {
    val D = """""""
    if ( includeZeros || counts.exists(_!=0) ) {
      output.println(D+label+D+" "+counts.mkString(" "))
    }
  }

  @scala.annotation.elidable(scala.annotation.elidable.CONFIG) private def count(i: Int) {
    counts(i) += 1
  }

  @scala.annotation.elidable(scala.annotation.elidable.CONFIG) private def newCounts() = {
    new Array[Int](logProbabilities.length)
  }

  @scala.annotation.elidable(scala.annotation.elidable.CONFIG) private def newRNG() = {
    new java.util.Random()
  }

  def apply(i: Int): Double = {
    count(i)
    logProbabilities(i)
  }

  override def toString = {
    logProbabilities.map(math.exp).mkString(" ")
  }

}

object ProbabilityMassFunction {

  def apply(probabilities: Array[Double], primary: Boolean) = {
    if ( probabilities.length < 0 ) throw new IllegalArgumentException("Dimension must be at least 1.")
    val length = if ( primary ) AAOrder.length else SSOrder.length
    if ( probabilities.length != length ) throw new IllegalArgumentException("Probability vector must be of length "+length+".")
    if ( (probabilities.sum-1.0).abs > 0.00000001 ) throw new IllegalArgumentException("Probability vector must sum to one.")
    val logProbabilities = probabilities.map(math.log)
    new ProbabilityMassFunction(logProbabilities)
  }

  def nextMultinomial(rng: java.util.Random, size: Int, probabilities: Array[Double]): Array[Int] = {
    def sampleWithSum(): Int = {
      val unif = rng.nextDouble
      var s = 1.0
      var i = probabilities.length - 1
      s -= probabilities(i)
      while ((i > 0) && (s >= unif)) {
        i -= 1
        s -= probabilities(i)
      }
      i
    }
    val sample = new Array[Int](probabilities.length)
    for (k <- 0 until size) {
      sample(sampleWithSum()) += 1
    }
    sample
  }

}


