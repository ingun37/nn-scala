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

  val f = new Differentiable{
    val func: List[Double] => List[Double] = x => x match {
      case x :: y :: _ => List(x * exp(x*y))
    }
    val derivative: List[Double] => Matrix = x => x match {
      case x :: y :: next => Matrix( List(List(exp(x*y) + y*x*exp(x*y), x*x*exp(x*y))))
    }
  }

  val g = new Differentiable {
    val func: List[Double] => List[Double] = x => x match {
      case t :: next => List(t*t, 1/t)
    }
    val derivative: List[Double] => Matrix = x => x match {
      case t :: next => Matrix(List(List(2*t),List(-pow(t,-2))))
    }
  }

  val fg = f.compose(g)
  println(fg.func(List(4)))
  println(fg.derivative(List(4)))
}