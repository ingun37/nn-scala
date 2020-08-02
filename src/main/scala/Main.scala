import cats.Semigroup
import cats.Apply
import cats.implicits._
import com.kubukoz.polskibus.domain.Matrix
import scala.collection.immutable.Nil
import scala.math.{exp, pow}

object Main extends App {
  case class NN (

  )
  type Vec = List[Double]
  trait WeightDifferentiable {
    val prevActivation: Vec
    val func: Matrix => Vec
    val derivative: Matrix => Matrix
  }
  trait Differentiable {
    val func: Vec => Vec
    val derivative: Vec => Matrix

    def compose(g: Differentiable): Differentiable = {
      val parent = this
      return new Differentiable {
        val func: Vec => Vec = parent.func compose g.func
        val derivative: Vec => Matrix = a =>
          parent.derivative(g.func(a)) * g.derivative(a)
      }
    }
    def compose(g: WeightDifferentiable): WeightDifferentiable = {
      val parent = this
      return new WeightDifferentiable {
        val prevActivation: Vec = g.prevActivation
        val func: Matrix => Vec = parent.func compose g.func
        val derivative: Matrix => Matrix = parent.derivative compose (_*prevActivation)
      }
    }
  }
  def sigmo(x:Double): Double = 1.0/(1.0+exp(-x))
  trait Sigmoid extends Differentiable {
    override val func: Vec => Vec = x => x match {
      case x :: next => List(sigmo(x))
    }
    override val derivative: Vec => Matrix = x => x match {
      case x :: next => Matrix(List(List(sigmo(x) * (1-sigmo(x)))))
    }
  }
}
