import Ndarray.*


@main
def main(): Unit = {
  val x1 = Varray(100)
  val x2 = Varray(100)
  val y = x1 + x2 + x1 * x2
  println(y.eval().data)
}

