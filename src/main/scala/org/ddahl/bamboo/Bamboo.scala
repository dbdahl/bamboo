package org.ddahl.bamboo

class Bamboo private (val ssBlocks: SSBlocks) {

  import Bamboo.{sample,oneDiff,twoDiff,toSequence}

  lazy val nBlocks = ssBlocks.size 
  lazy val nPositions = ssBlocks.map(_._2).reduce(_+_)
  lazy val asSequence = toSequence(ssBlocks)
  lazy val asString = ssBlocks.map(x => x._1*x._2).mkString
  override def toString = asString

  private val allowKOS = ssBlocks.forall(x => x._1 == "K" || x._1 == "O" || x._1 == "S" )
  private val possibilities = if ( allowKOS ) Map("K"->1.0,"O"->1.0,"S"->1.0)
                              else Map("H"->1.0,"E"->1.0,"T"->1.0,"C"->1.0)
  private val minLengths = if ( allowKOS ) Map("K"->1,"O"->1,"S"->1)
                           else Map("H"->3,"E"->3,"T"->3,"C"->1)
  if ( possibilities.keys.toSet != minLengths.keys.toSet ) throw new RuntimeException("Mismatch in possibilities and minLengths.")

  if ( ( nBlocks > 0 ) && ( ! allowKOS ) ) {
    if ( ssBlocks.head._1 != "C" ) throw new IllegalArgumentException("Sequence must begin in coil: "+asString+", "+ssBlocks)
    if ( ssBlocks.last._1 != "C" ) throw new IllegalArgumentException("Sequence must end in coil: "+asString+", "+ssBlocks)
    if ( ssBlocks.forall(_._1 == "C") ) throw new IllegalArgumentException("Sequence cannot be all coil: "+asString+", "+ssBlocks)
    ssBlocks.foreach(x => {
      if ( ! possibilities.contains(x._1) ) throw new IllegalArgumentException("Block types must be "+possibilities.keys.mkString(", ")+": "+asString+", "+ssBlocks)
      val min = minLengths(x._1)
      if ( x._2 < min ) throw new IllegalArgumentException("Blocks must have minimum lengths: "+asString)
    })
  }

  if ( nBlocks > 0 ) {
    (ssBlocks :+ (" ",0), (" ",0) +: ssBlocks).zipped.foreach( (x,y) => {
      if ( x._1 == y._1 ) throw new IllegalArgumentException("Consecutive blocks be of different types.")
    })
  }

  def sameInstanceAs(other: Bamboo) = this eq other

  def propose(moves: Map[String,Double] = Map("switch" -> 1, "reposition" -> 1, "split" -> 1, "merge" -> 1), fromBamboo: Bamboo = null): Tuple2[Bamboo,Double] = {
    val (nB, fBamboo,tBamboo) = if ( fromBamboo == null ) (nBlocks,ssBlocks,null) else (fromBamboo.nBlocks,fromBamboo.ssBlocks,ssBlocks)
    val m = if ( nB == 1 ) moves.filterNot( x => x._1 == "reposition" || x._1 == "merge" ) else moves
    sample(m) match {
      case "switch" =>
        val blockIndex = sample(nB)
        val block = fBamboo(blockIndex)
        var poss = possibilities - block._1
        if ( blockIndex > 0 ) poss -= fBamboo(blockIndex-1)._1
        if ( blockIndex < nB-1 ) poss -= fBamboo(blockIndex+1)._1
        try {
          (Bamboo(fBamboo.updated(blockIndex,(sample(poss),block._2))),0.0)
        } catch {
          case _ : Throwable => return (this,0.0)
        }
      case "reposition" =>
        val blockIndex = sample(nB-1)
        val blocks = fBamboo.slice(blockIndex,blockIndex+2)
        val block1 = blocks.head
        val block2 = blocks.tail.head
        val sum = block1._2 + block2._2
        val aLength = sample(sum-1) + 1
        val bLength = sum - aLength
        val newBlocks = List((block1._1,aLength),(block2._1,bLength))
        try {
          (Bamboo(fBamboo.patch(blockIndex,newBlocks,2)),0.0)
        } catch {
          case _ : Throwable => return (this,0.0)
        }
      case "split" =>
        var lpp = 0.0
        val (blockIndex,block) = if ( tBamboo == null ) {
          val bi = sample(nB)
          (bi,fBamboo(bi))
        } else oneDiff(0,fBamboo,tBamboo)
        if ( block._2 > 1 ) {
          val tNewBlocks = if ( tBamboo == null ) null else twoDiff(0,tBamboo,fBamboo)
          lpp -= math.log(nB)
          val aLength = if ( tBamboo == null ) sample(block._2-1) + 1 else tNewBlocks._2._2
          lpp -= math.log(block._2-1)
          val bLength = block._2 - aLength
          var poss = possibilities - block._1
          if ( blockIndex < nB-1 ) poss -= fBamboo(blockIndex+1)._1
          val newType = if ( tBamboo == null ) sample(poss) else tNewBlocks._3._1
          lpp -= math.log(poss.size)
          val newBlocks = List((block._1,aLength),(newType,bLength))
          val proposal = try {
            Bamboo(fBamboo.patch(blockIndex,newBlocks,1))
          } catch {
            case _ : Throwable => return (this,0.0)
          }
          if ( tBamboo == null ) {
            val reverse = propose(Map("merge"->1),proposal)
            (proposal,reverse._2-lpp)
          } else (proposal,lpp)
        } else return (this,0.0)
      case "merge" =>
        var lpp = 0.0
        val (blockIndex:Int,block1:Block,block2:Block) = if ( tBamboo == null ) {
          val bi = sample(nB-1)
          val blocks = fBamboo.slice(bi,bi+2)
          val b1 = blocks.head
          val b2 = blocks.tail.head
          if ( ( bi + 2 < nB ) && ( b1._1 == fBamboo(bi+2) ) ) return (this,0.0)
          (bi,blocks.head,blocks.tail.head)
        } else twoDiff(0,fBamboo,tBamboo)
        lpp -= math.log(nB-1)
        val proposal = try {
          Bamboo(fBamboo.patch(blockIndex,List((block1._1,block1._2+block2._2)),2))
        } catch {
          case _ : Throwable => return (this,0.0)
        }
        if ( tBamboo == null ) {
          val reverse = propose(Map("split"->1),proposal)
          (proposal,reverse._2-lpp)
        } else (proposal,lpp)
      case _ : String => throw new IllegalArgumentException("Unrecognized move type.")
    }
  }

}

object Bamboo {

  def apply(x: SSBlocks) = new Bamboo(x)

  def apply(x: Block*): Bamboo = new Bamboo(List(x: _*))

  def apply(x: String): Bamboo = new Bamboo(toSSBlocks(x))

  private val BlockReg = """^(H+|E+|T+|C+|K+|O+|S+)(.*)$""".r
  def toSSBlocks(x: String): SSBlocks = {
    var yy: SSBlocks = List()
    var xx = x.replace("L","C").reverse
    while ( xx.length > 0 ) {
      xx match {
        case BlockReg(block,residue) =>
          val tt = block(0).toString
          val len = block.length
          yy = (tt,len) :: yy
          xx = residue
        case e => throw new IllegalArgumentException("Violation of secondary structure syntax: "+x)
      }
    }
    yy
  }

  def toReliability(x: String): List[Double] = x.split("").tail.map(_.toDouble).toList

  def toSequence(ssBlocks: SSBlocks) = {
    ssBlocks.map(x => List.fill(x._2) { x._1 }).reduce(_++_)
  }

  def enumerate(length: Int, possibilities: Iterable[String]) = {
    var candidates = List[SSBlocks]()
    def expand(fixedSSSeq: SSBlocks, fixedLength: Int, block: Block): Unit = {
      if ( fixedLength + block._2 == length ) {
        val candidate = fixedSSSeq :+ block
        try {
          Bamboo(candidate)
          candidates = candidate +: candidates
        } catch {
          case _ : Throwable =>
        }
      } else {
        expand(fixedSSSeq,fixedLength,(block._1,block._2+1))
        possibilities.filter(_ != block._1).foreach( t => expand(fixedSSSeq :+ block, fixedLength + block._2, (t,1)) )
      }
    }
    val empty: SSBlocks = List()
    expand(empty,0,("C",1))
    candidates
  }

  def sample[T](weights: Map[T,Double]) = {
    val cum = computeCummulativeTable(weights)
    sampleWithTable(cum,cum.last._2)
  }

  def samplePair(upper: Int): Tuple2[Int,Int] = {
    val x = sample(upper)
    val y = sample(upper-1)
    if ( y >= x ) (x,y+1) else (x,y)
  }

  def sample(upper: Int): Int = {
    (math.random * upper).floor.toInt
  }

  private def computeCummulativeTable[T](weights: Map[T,Double]) = {
    val w = weights.toList
    w.tail.scanLeft(w.head) { (x,y) => (y._1,y._2+x._2) }
  }

  private def sampleWithTable[T](cummulative: List[Tuple2[T,Double]], max: Double): T = {
    val v = math.random * max
    cummulative.find( x => v <= x._2 ).get._1
  }

  private def oneDiff(index: Int, a: SSBlocks, b: SSBlocks): Tuple2[Int,Block] = {
    if ( ! ( a.isEmpty || b.isEmpty ) && ( a.head == b.head ) ) oneDiff(index+1,a.tail,b.tail)
    else (index,a.head)
  }

  private def twoDiff(index: Int, a: SSBlocks, b: SSBlocks): Tuple3[Int,Block,Block] = {
    if ( ! ( a.isEmpty || b.isEmpty ) && ( a.head == b.head ) ) twoDiff(index+1,a.tail,b.tail)
    else {
      (index,a.head,a.tail.head)
    }
  }

}
