package chapter4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(value) => Right(f(value))
    case Left(value) => Left(value)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case Left(value) => Left(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case r: Right[A] => this
    case _: Left[E] => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    for { a1 <- this; b1 <- b } yield f(a1, b1)

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case head :: tail => head.flatMap(h => sequence(tail).map(h :: _))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case head :: tail => f(head).map2(traverse(tail)(f))(_ :: _)
  }

}