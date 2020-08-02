import cats.Semigroup
import cats.Apply
import cats.implicits._
import com.kubukoz.polskibus.domain.Matrix
import scala.collection.immutable.Nil
import scala.math.{exp, pow, cos, sin, Pi}
import Main.Differentiable

class CubeCalculatorTest extends org.scalatest.FunSuite {
  test("chain rule") {
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
  test("chain rule 2") {
    val f = new Differentiable {
      val func: List[Double] => List[Double] = x => {
        x match {
          case x :: y :: z :: next => List(pow(x,3)*y*z + x*y + z + 3)
        }
      }
      
      val derivative: List[Double] => Matrix = x => {
        x match {
          case x :: y :: z :: next => Matrix(List(List(3*pow(x,2)*y*z+y, pow(x,3)*z+x, pow(x,3)*y+1)))
        }
      }
    }

    val g = new Differentiable {
      val func: List[Double] => List[Double] = x => {
        x match {
          case t :: next => List(3*cos(t), 3*sin(t), 2*t)
        }
      }
      
      val derivative: List[Double] => Matrix = x => {
        x match {
          case t :: next => Matrix(List(List(-3*sin(t)), List(3*cos(t)), List(2)))
        }
      }
    }

    val fg = f compose g

    assert(fg.derivative(List(Pi/2)) == Matrix(List(List(-7))))
  }
  test("chain rule 3") {
    val f = new Differentiable {
      val func: List[Double] => List[Double] = x => throw new Exception("no need to implement")
      val derivative: List[Double] => Matrix = x => x match {
        case 2 :: -1 :: next => Matrix(List(List(3,-2)))
        case aaa => Matrix(List(List(-333,-333)))
      }
    }
    val g = new Differentiable {
      val func: List[Double] => List[Double] = x => x match {
        case x :: y :: next => List(x*x*y, x-y)
      }
      
      val derivative: List[Double] => Matrix = x => x match {
        case x :: y :: next => Matrix(List(List(2*x*y, x*x), List(1, -1)))
      }
    }
    val fg = f compose g
    assert(fg.derivative(List(1,2)) == Matrix(List(List(10,5))))
  }

  test("transpose") {
    val m = Matrix(List(List(1,2)))
    assert(m.transpose == Matrix(List(List(1), List(2))))
  }

  test("multiply by vector") {
    val m = Matrix(List(List(1,2,3), List(4,5,6)))
    val v:List[Double] = List(7,8,9)
    assert(m * v == List[Double](50,122))
  }
}
