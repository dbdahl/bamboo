package org.ddahl.bamboo

abstract class BlockLikelihood {

  def apply(antecedent: AminoAcidSequence, segment: AminoAcidSequence, descendant: AminoAcidSequence): Double

  def reverse: BlockLikelihood

  def countsDumpEngine(output: java.io.PrintWriter): Unit

  def countsDump(directory: String, filename: String): Unit = {
    import java.io._
    val D = """""""
    val header = D+AAOrder.mkString(D+" "+D)+D
    val output = new PrintWriter(new BufferedWriter(new FileWriter(new File(directory,filename))))
    output.println(header)
    countsDumpEngine(output)
    output.close
  }

}

