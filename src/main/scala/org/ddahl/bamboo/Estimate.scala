package org.ddahl.bamboo

// Is there every time when the proposal is the same as the current but ! ( proposal eq current ) ?
// If so, we could avoid computing the likelihood all over again.

case class MCMCResults(countTotal: Int, countBad: Int, countAccepted: Int, mapState: SSBlocks, maxLogPosterior: Double, mpState: SSBlocks, lsState: SSBlocks, marginalProbabilities: Array[Array[Double]])

object Estimate {

  def apply(likelihood: Likelihood, prior: Prior, initialState: Bamboo, nSamples: Int, dropFirst: Int, doLeastSquareEstimation: Boolean, dumpStates: Boolean) = {
    if ( likelihood.aa.nPositions != initialState.nPositions ) throw new IllegalArgumentException("Inconsistent number of positions.")
    if ( nSamples < 1 ) throw new IllegalArgumentException("Number of samples must be at least one.")
    if ( dropFirst >= nSamples ) throw new IllegalArgumentException("Number to drop must be less than number of samples.")
    def posterior(state: Bamboo) = likelihood(state) + prior(state)
    var countBad = 0
    var countAccepted = 0
    var i = 0
    var current = initialState
    var logPosteriorCurrent = posterior(current)
    var mapState = current
    var maxLogPosterior = logPosteriorCurrent
    var visitedStates = new scala.collection.mutable.HashSet[SSBlocks]()
    val marginalCounts = Array.ofDim[Int](likelihood.aa.nPositions,SSOrder.length)
    while ( i < nSamples ) {
      val (proposal,logRatioProposalProbability) = current.propose()
      if ( ! ( proposal eq current ) ) {
        val logPosteriorProposal = posterior(proposal)
        val logMHRatio = logPosteriorProposal - logPosteriorCurrent + logRatioProposalProbability
        if ( math.log(math.random) < logMHRatio ) {
          countAccepted += 1
          current = proposal
          logPosteriorCurrent = logPosteriorProposal
          if ( maxLogPosterior < logPosteriorCurrent ) {
            mapState = current
            maxLogPosterior = logPosteriorCurrent
          }
        }
      } else {
        countBad += 1
      }
      i += 1
      if ( i >= dropFirst ) {
        if ( i == dropFirst ) {
          countBad = 0
          countAccepted = 0
        } else {
          current.asSequence.map(SSIndexMap(_)).zipWithIndex.foreach( pair => {
            marginalCounts(pair._2)(pair._1) += 1
          })
          if ( dumpStates ) println(current)
          if ( doLeastSquareEstimation ) visitedStates.add(current.ssBlocks)
        }
      }
    }
    val countTotal = i-dropFirst
    val marginalProbabilities = marginalCounts.map(_.map(_.toDouble/countTotal))
    val mpState = Bamboo.toSSBlocks(marginalProbabilities.map( x => {
        val max = x.max
        x.indexWhere( _ == max )
      }).map(SSOrder(_)).mkString("")
    )
    var lsState = mpState
    if ( doLeastSquareEstimation ) {
      var minLS = Double.MaxValue
      visitedStates.foreach( x => {
        var sum = 0.0
        var i = 0
        x.foreach( y => {
          var index = SSIndexMap(y._1)
          var j = 0
          while ( j < y._2 ) {
            sum += math.pow(1 - marginalProbabilities(i+j)(index),2)
            j += 1
          }
          i += j
        })
        if ( sum < minLS ) {
          minLS = sum
          lsState = x
        }
      })
    }
    MCMCResults(countTotal,countBad,countAccepted,mapState.ssBlocks,maxLogPosterior,mpState,lsState,marginalProbabilities)
  }

}

