import cats.Semigroup
import cats.Apply
import cats.implicits._
import com.kubukoz.polskibus.domain.Matrix
import scala.collection.immutable.Nil
import scala.math.{exp, pow}

object Main extends App {
  type Vec = List[Double]
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
  }
  // trait WeightDifferentiable {
  //   val prevActivation: Vec
  //   val func: Matrix => Vec = (m)=>
  // }
}
