package org.ddahl.bamboo

class PriorMM5 private (
    val p0: ProbabilityMassFunction,
    val p1: Conditional1PMF,
    val p2: Conditional2PMF,
    val p3: Conditional3PMF,
    val p4: Conditional4PMF,
    val p5: Conditional5PMF
  ) extends Prior {

  def apply(state: Bamboo): Double = {
    val t = state.ssBlocks.map(_._1).map(x => SSIndexMap(x.toString)).toArray
    val k = t.length
    var sum = p0(t(0))
    if ( k > 1 ) {
      sum += p1(t(0))(t(1))
      if ( k > 2 ) {
        sum += p2(t(0),t(1))(t(2))
        if ( k > 3 ) {
          sum += p3(t(0),t(1),t(2))(t(3))
          if ( k > 4 ) {
            sum += p4(t(0),t(1),t(2),t(3))(t(4))
            if ( k > 5 ) {
              var i = 5
              while ( i < k ) {
                sum += p5(t(i-5),t(i-4),t(i-3),t(i-2),t(i-1))(t(i))
                i += 1
              }
            }
          }
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
    p3.countsDump(output,"3",false)
    p4.countsDump(output,"4",false)
    p5.countsDump(output,"5",false)
    output.close
  }

}

object PriorMM5 {

  def apply(
    p0: ProbabilityMassFunction,
    p1: Conditional1PMF,
    p2: Conditional2PMF,
    p3: Conditional3PMF,
    p4: Conditional4PMF,
    p5: Conditional5PMF
  ): PriorMM5 = new PriorMM5(p0,p1,p2,p3,p4,p5)

  def apply(bag: BagOfPMFs): PriorMM5 = {
    val p0 = bag._0("0")
    val p1 = bag._1("1")
    val p2 = bag._2("2")
    val p3 = bag._3("3")
    val p4 = bag._4("4")
    val p5 = bag._5("5")
    apply(p0,p1,p2,p3,p4,p5)
  }

}

