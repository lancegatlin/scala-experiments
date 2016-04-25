ckage s_mach.validate

import scala.language.higherKinds
import scalaz._
import Id._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._

object test {
//  trait Monad[M[_]] {
//    def map[A,B](m: M[A], f: A => B) : M[B]
//    def flatMap[A,B](m: M[A],f: A => M[B]) : M[B]
//    def apply[A](a: A) : M[A]
//  }

//  type Monadic[A,MDT[AA] <: Monadic[AA,MDT]] = {
//    def map[B](f: A => B) : MDT[B]
//    def flatMap[B](f: A => MDT[B]) : MDT[B]
//  }

  implicit class PimpMyMonad[M[_],A](val self: M[A]) extends AnyVal {
    def map[B](f: A => B)(implicit M:Monad[M]) : M[B] =
      M.map(self)(f)
    def flatMap[B](f: A => M[B])(implicit M:Monad[M]) : M[B] =
      M.bind(self)(f)
  }

  abstract class Liftable[M1[_]:Monad,M2[_]:Monad] {
    def lift[A](m:M1[A]) : M2[A]
  }
  object Liftable {
    def apply[M1[_],M2[_]](implicit L:Liftable[M1,M2]) = L
  }

  trait Logger[E[_]] { self =>
    def info(msg: => String) : E[Unit]

    def lift[E1[_]](implicit l:Liftable[E,E1]) =
      new Logger[E1] {
        override def info(msg: => String): E1[Unit] =
          l.lift(self.info(msg))
      }
  }

  trait Console[E[_]] { self =>
    def print(msg: String) : E[Unit]

    def lift[E1[_]](implicit l:Liftable[E,E1]) =
      new Console[E1] {
        override def print(msg: String): E1[Unit] =
          l.lift(self.print(msg))
      }
  }

  trait MyService[E[_]] { self =>
    def myMethod(i: Int) : E[String]

    def lift[E1[_]](implicit l:Liftable[E,E1]) =
      new MyService[E1] {
        override def myMethod(i: Int): E1[String] =
          l.lift(self.myMethod(i))
      }
  }

//  type Id[A] = A
//
//  implicit object monad_Id extends Monad[Id] {
//    override def point[A](a: => A): Id[A] = a
//    override def bind[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = f(fa)
//  }

  object ImmediateLogger extends Logger[Id] {
    override def info(msg: => String): Id[Unit] =
      println(s"[INFO] $msg")
  }

  object ImmediateConsole extends Console[Id] {
    override def print(msg: String): Id[Unit] =
      println(msg)
  }

  implicit def liftable_Id[M[_]](implicit M:Monad[M]) = new Liftable[Id,M] {
    def lift[A](m:Id[A]) : M[A] = {
      M.point(m)
    }
  }

  trait MyDb[E[_]] { self =>
    def read() : E[Int]

    def lift[E1[_]](implicit L:Liftable[E,E1]) =
      new MyDb[E1] {
        override def read(): E1[Int] =
          L.lift(self.read())
      }
  }

  object MyDbImpl extends MyDb[Future] {
    override def read(): Future[Int] = Future {
      Thread.sleep(3000)
      2
    }
  }

  class MyServiceImpl[E[_]](
    db: MyDb[E],
    logger: Logger[E],
    console: Console[E]
  )(implicit
    E:Monad[E]
  ) extends MyService[E] {
    override def myMethod(i: Int): E[String] = {
      for {
        j <- db.read()
        _ <- logger.info(s"read $j from db")
        k = 2
        _ <- console.print(s"i=$i j=$j k=$k")
      } yield (i * j * k).toString
    }
  }

//  sealed trait Free[+A] {
//    def eval() : A
//  }
//  object Free {
//    case class Point[+A](value: A) extends Free[A] {
//      override def eval(): A = value
//    }
//    case class Map[+A,+B](fa: Free[A], f: A => B) extends Free[B] {
//      override def eval(): B = f(fa.eval())
//    }
//    case class FlatMap[+A,+B](fa: Free[A], f: A => Free[B]) extends Free[B] {
//      override def eval(): B = f(fa.eval()).eval()
//    }
//  }
//
//  implicit object monad_Free extends Monad[Free] {
//    override def apply[A, B](fa: Free[A])(f: (A) => B): Free[B] =
//      Free.Map(fa, f)
//    override def point[A](a: => A): Free[A] =
//      Free.Point(a)
//    override def bind[A, B](fa: Free[A])(f: (A) => Free[B]): Free[B] =
//      Free.FlatMap(fa, f)
//  }
  implicit object monad_Future extends Monad[Future] {
    override def map[A, B](fa: Future[A])(f: (A) => B): Future[B] = fa.map(f)
    override def apply[A, B](fa: Future[A])(f: (A) => B): Future[B] = fa.map(f)
    override def point[A](a: => A): Future[A] = Future.successful(a)
    override def bind[A, B](fa: Future[A])(f: (A) => Future[B]): Future[B] = fa.flatMap(f)

  }

  val svc =
    new MyServiceImpl[Future](
      MyDbImpl,
      ImmediateLogger.lift,
      ImmediateConsole.lift
    )
//
//  implicit object monad_Option extends Monad[Option] {
//    override def map[A, B](m: Option[A], f: (A) => B): Option[B] =
//      m.map(f)
//
//    override def flatMap[A, B](m: Option[A], f: (A) => Option[B]): Option[B] =
//      m.flatMap(f)
//    override def apply[A](a: A): Option[A] =
//      Some(a)
//  }
//
//  type Id[A] = A
//  implicit object monad_Identity extends Monad[Id] {
//    override def map[A, B](m: Id[A], f: (A) => B): Id[B] = f(m)
//
//    override def flatMap[A, B](m: Id[A], f: (A) => Id[B]): Id[B] = f(m)
//
//    override def apply[A](a: A): Id[A] = a
//  }
}

