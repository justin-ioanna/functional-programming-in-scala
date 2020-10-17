package chapter6

case class State[S, +A](run: S => (A, S)) {

  /**
   * Exercise 6.10
   */
  def map[B](f: A => B): State[S, B] =
   flatMap(a => unit(f(a)))

  /**
   * Exercise 6.10
   */
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  /**
   * Exercise 6.10
   */
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

}

object State {

  /**
   * Exercise 6.10
   */
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

}