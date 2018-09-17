package org.ddahl.bamboo

class BagOfPMFs private (map: Map[String,ProbabilityMassFunction], primary: Boolean) {

  private val order = if ( primary ) AAOrder else SSOrder 

  def _0(label: String) = map(label)

  def _1(label: String) = {
    Conditional1PMF(order.map(x1 => map(label+x1)),primary)
  }

  def _2(label: String) = {
    Conditional2PMF(order.map(x2 => order.map(x1 => map(label+x2+x1))),primary)
  }

  def _3(label: String) = {
    Conditional3PMF(order.map(x3 => order.map(x2 => order.map(x1 => map(label+x3+x2+x1)))),primary)
  }

  def _4(label: String) = {
    Conditional4PMF(order.map(x4 => order.map(x3 => order.map(x2 => order.map(x1 => map(label+x4+x3+x2+x1))))),primary)
  }

  def _5(label: String) = {
    Conditional5PMF(order.map(x5 => order.map(x4 => order.map(x3 => order.map(x2 => order.map(x1 => map(label+x5+x4+x3+x2+x1)))))),primary)
  }

  def _6(label: String) = {
    Conditional6PMF(order.map(x6 => order.map(x5 => order.map(x4 => order.map(x3 => order.map(x2 => order.map(x1 => map(label+x6+x5+x4+x3+x2+x1))))))),primary)
  }

  def _7(label: String) = {
    Conditional7PMF(order.map(x7 => order.map(x6 => order.map(x5 => order.map(x4 => order.map(x3 => order.map(x2 => order.map(x1 => map(label+x7+x6+x5+x4+x3+x2+x1)))))))),primary)
  }

  def _8(label: String) = {
    Conditional8PMF(order.map(x8 => order.map(x7 => order.map(x6 => order.map(x5 => order.map(x4 => order.map(x3 => order.map(x2 => order.map(x1 => map(label+x8+x7+x6+x5+x4+x3+x2+x1))))))))),primary)
  }

  def _9(label: String) = {
    Conditional9PMF(order.map(x9 => order.map(x8 => order.map(x7 => order.map(x6 => order.map(x5 => order.map(x4 => order.map(x3 => order.map(x2 => order.map(x1 => map(label+x9+x8+x7+x6+x5+x4+x3+x2+x1)))))))))),primary)
  }

}

object BagOfPMFs {

  private def convertMap(map: scala.collection.mutable.Map[String,ProbabilityMassFunction], primary: Boolean) = {
    map.toMap.withDefault( (key:String) => {
      val m = if ( primary ) ProbabilityMassFunction(Array.fill(AAIndexMap.size)(1.0/AAIndexMap.size),primary)
      else ProbabilityMassFunction(Array.fill(SSIndexMap.size)(1.0/SSIndexMap.size),primary)
      if ( m.training == false ) throw new RuntimeException("Key not found when not training!")
      m
    })
  }

  def apply(filename: String, primary: Boolean) = {
    import java.io._
    val baseMap = if ( primary ) AAIndexMap else SSIndexMap
    val reader = new BufferedReader(new FileReader(filename))
    val header = reader.readLine.replaceAll(""""""","").split("""\s+""")
    val permutation = header.map(baseMap(_)).zipWithIndex.sortWith(_._1 < _._1).map(_._2)
    var line = reader.readLine
    val map = scala.collection.mutable.HashMap[String,ProbabilityMassFunction]()
    while ( line != null ) {
      val cells = line.replaceAll(""""""","").split("""\s+""")
      val tail = cells.tail
      val (probs,size) = if ( tail.exists(_.contains(".")) ) {
        (tail.map(_.toDouble),0)
      } else {
        val counts = tail.map(_.toInt+1)     // Prior is worth "length" observations
        val sum = counts.sum
        (counts.map(_.toDouble/sum),sum-baseMap.size)
      }
      map(cells(0)) = ProbabilityMassFunction(permutation.map(probs(_)),primary)
      line = reader.readLine
    }
    reader.close
    new BagOfPMFs(convertMap(map,primary),primary)
  }

  def apply(primary: Boolean) = {
    new BagOfPMFs(convertMap(scala.collection.mutable.Map[String,ProbabilityMassFunction](),primary),primary)
  }

}

