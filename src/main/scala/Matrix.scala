package com.kubukoz.polskibus.domain

import scala.language.postfixOps

case class Matrix(data: List[List[Double]]) {
  final lazy val valid = {
    require(data.forall(s => s.size == data.head.size), "All rows must have the same column amount")
    true
  }

  val rows = data.size
  val columns = data.head.size

  def *(b: Matrix): Matrix = Matrix(data.indices map { i =>
      b.data.head.indices map { j =>
        (data.head.indices map { k =>
          data(i)(k) * b.data(k)(j)
        }) sum
      } toList
    } toList)

  override def toString: String = {
    valid
    data.map(_.mkString(" | ")).mkString("\n")
  }
}
