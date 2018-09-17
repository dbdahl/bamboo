package org.ddahl.bamboo

class PriorMM2 private (
    val p0: ProbabilityMassFunction,
    val p1: Conditional1PMF,
    val p2: Conditional2PMF
  ) extends Prior {

  def apply(state: Bamboo): Double = {
    val t = state.ssBlocks.map(_._1).map(x => SSIndexMap(x.toString)).toArray
    val k = t.length
    var sum = p0(t(0))
    if ( k > 1 ) {
      sum += p1(t(0))(t(1))
      if ( k > 2 ) {
        var i = 2
        while ( i < k ) {
          sum += p2(t(i-2),t(i-1))(t(i))
          i += 1
        }
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
    p2.countsDump(output,"2",false)
    output.close
  }

}

object PriorMM2 {

  def apply(
    p0: ProbabilityMassFunction,
    p1: Conditional1PMF,
    p2: Conditional2PMF
  ): PriorMM2 = new PriorMM2(p0,p1,p2)

  def apply(bag: BagOfPMFs): PriorMM2 = {
    val p0 = bag._0("0")
    val p1 = bag._1("1")
    val p2 = bag._2("2")
    apply(p0,p1,p2)
  }

}

