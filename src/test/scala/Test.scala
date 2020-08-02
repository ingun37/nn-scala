import cats.Semigroup
import cats.Apply
import cats.implicits._
import com.kubukoz.polskibus.domain.Matrix
import scala.collection.immutable.Nil
import scala.math.{exp, pow}
import Main.Differentiable

class CubeCalculatorTest extends org.scalatest.FunSuite {
  test("aoeu") {
    val f = new Differentiable {
      val func: List[Double] => List[Double] = x =>
        x match {
          case x :: y :: _ => List(x * exp(x * y))
        }
      val derivative: List[Double] => Matrix = x =>
        x match {
          case x :: y :: next =>
            Matrix(
              List(List(exp(x * y) + y * x * exp(x * y), x * x * exp(x * y)))
            )
        }
    }

    val g = new Differentiable {
      val func: List[Double] => List[Double] = x =>
        x match {
          case t :: next => List(t * t, 1 / t)
        }
      val derivative: List[Double] => Matrix = x =>
        x match {
          case t :: next => Matrix(List(List(2 * t), List(-pow(t, -2))))
        }
    }

    val fg = f.compose(g)
    assert(fg.func(List(4)) == List(873.5704005303078))
    assert(fg.derivative(List(4)) == Matrix(List(List(1310.3556007954617))))
  }
  test("htns") {}
}
