package org.ddahl.bamboo

class PriorMM1 private (
    val p0: ProbabilityMassFunction,
    val p1: Conditional1PMF
  ) extends Prior {

  def apply(state: Bamboo): Double = {
    val t = state.ssBlocks.map(_._1).map(x => SSIndexMap(x.toString)).toArray
    val k = t.length
    var sum = p0(t(0))
    if ( k > 1 ) {
      var i = 1
      while ( i < k ) {
        sum += p1(t(i-1))(t(i))
        i += 1
      }
    }
    sum
  }

  def count(ss: Array[String], filename: String): Unit = {
    ss.foreach(x => apply(Bamboo(x)))
    val D = """""""
    val header = D+SSOrder.mkString(D+" "+D)+D
    import java.io._
    val dir = (new File(filename)).getAbsoluteFile
    dir.getParentFile.mkdirs
    val output = new PrintWriter(new BufferedWriter(new FileWriter(new File(filename))))
    output.println(header)
    p0.countsDump(output,"0",false)
    p1.countsDump(output,"1",false)
    output.close
  }

}

object PriorMM1 {

  def apply(
    p0: ProbabilityMassFunction,
    p1: Conditional1PMF
  ): PriorMM1 = new PriorMM1(p0,p1)

  def apply(bag: BagOfPMFs): PriorMM1 = {
    val p0 = bag._0("0")
    val p1 = bag._1("1")
    apply(p0,p1)
  }

}

