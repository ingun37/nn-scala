import cats.Semigroup
import cats.Apply
import cats.implicits._
import com.kubukoz.polskibus.domain.Matrix
import scala.collection.immutable.Nil
import scala.math.{exp,pow}

object Main extends App {
  trait Differentiable {
    val func: List[Double] => List[Double]
    val derivative: List[Double] => Matrix

    def compose(g:Differentiable) : Differentiable = {
      val parent = this
      return new Differentiable {
        val func: List[Double] => List[Double] = parent.func compose g.func
        val derivative: List[Double] => Matrix = a => parent.derivative(g.func(a)) * g.derivative(a)
      } 
    }
  }

  
}