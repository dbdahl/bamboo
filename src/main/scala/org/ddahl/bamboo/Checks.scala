package org.ddahl.bamboo

class Checks {

  protected def check1(x: Array[ProbabilityMassFunction], length: Int) = {
    if ( x.length != length ) throw new IllegalArgumentException("Dimension of multinomial vector must be of length "+length+".")
  }

  protected def check2(x: Array[Array[ProbabilityMassFunction]], length: Int) = {
    if ( x.length != length ) throw new IllegalArgumentException("Dimension of multinomial vector must be of length "+length+".")
    for ( y <- x ) check1(y,length)
  }

  protected def check3(x: Array[Array[Array[ProbabilityMassFunction]]], length: Int) = {
    if ( x.length != length ) throw new IllegalArgumentException("Dimension of multinomial vector must be of length "+length+".")
    for ( y <- x ) check2(y,length)
  }

  protected def check4(x: Array[Array[Array[Array[ProbabilityMassFunction]]]], length: Int) = {
    if ( x.length != length ) throw new IllegalArgumentException("Dimension of multinomial vector must be of length "+length+".")
    for ( y <- x ) check3(y,length)
  }

  protected def check5(x: Array[Array[Array[Array[Array[ProbabilityMassFunction]]]]], length: Int) = {
    if ( x.length != length ) throw new IllegalArgumentException("Dimension of multinomial vector must be of length "+length+".")
    for ( y <- x ) check4(y,length)
  }

  protected def check6(x: Array[Array[Array[Array[Array[Array[ProbabilityMassFunction]]]]]], length: Int) = {
    if ( x.length != length ) throw new IllegalArgumentException("Dimension of multinomial vector must be of length "+length+".")
    for ( y <- x ) check5(y,length)
  }

  protected def check7(x: Array[Array[Array[Array[Array[Array[Array[ProbabilityMassFunction]]]]]]], length: Int) = {
    if ( x.length != length ) throw new IllegalArgumentException("Dimension of multinomial vector must be of length "+length+".")
    for ( y <- x ) check6(y,length)
  }

  protected def check8(x: Array[Array[Array[Array[Array[Array[Array[Array[ProbabilityMassFunction]]]]]]]], length: Int) = {
    if ( x.length != length ) throw new IllegalArgumentException("Dimension of multinomial vector must be of length "+length+".")
    for ( y <- x ) check7(y,length)
  }

  protected def check9(x: Array[Array[Array[Array[Array[Array[Array[Array[Array[ProbabilityMassFunction]]]]]]]]], length: Int) = {
    if ( x.length != length ) throw new IllegalArgumentException("Dimension of multinomial vector must be of length "+length+".")
    for ( y <- x ) check8(y,length)
  }

}

