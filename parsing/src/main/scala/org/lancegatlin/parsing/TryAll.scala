package org.lancegatlin.parsing

import scala.collection.Seq
import scala.util.control.NonFatal
import scala.language.implicitConversions
import scala.util.Try

/**
 * The `TryAll` type represents a computation that may either result in a list of exceptions, or return a
 * successfully computed value. It's similar to, but semantically different from the [[scala.util.Try]] and
 * [[scala.util.Either]] type.
 *
 * Instances of `TryAll[T]`, are either an instance of [[scala.util.Success]][T] or [[scala.util.Failure]][T].
 *
 * For example, `TryAll` can be used to accumulate exceptions to ensure that all exceptions that might occur are
 * returned when normally only the first exception would be returned. In the example below, without TryAll, only the
 * first failing require would be thrown. Utilizing TryAll, all require calls below will be executed and any exceptions
 * thrown will be accumulated and finally rethrown as Exceptions(exception).
 *
 * Example:
 * {{{
 *   case class Person(firstName: String, middleName: String, lastName: String, age: Int) {
 *     (for {
 *        _ <- TryAll(require(firstName != null && firstName.length > 0, "firstName must be set"))
 *        _ <- TryAll(require(middleName != null && middleName.length > 0, "middleName must be set"))
 *        _ <- TryAll(require(lastName != null && lastName.length > 0, "lastName must be set"))
 *        _ <- TryAll(require(0 < age && age < 150, "age must be between 0 and 150"))
 *      } yield ()).throwIfFailure
 *  }
 *
 *  // Same code rewritten using tryAll convenience method
 *  case class Person(firstName: String, middleName: String, lastName: String, age: Int) {
 *     tryAll(
 *        require(firstName != null && firstName.length > 0, "firstName must be set"),
 *        require(middleName != null && middleName.length > 0, "middleName must be set"),
 *        require(lastName != null && lastName.length > 0, "lastName must be set"),
 *        require(0 < age && age < 150, "age must be between 0 and 150"),
 *      ).throwIfFailure
 *  }
 * }}}
 *
 * An important property of `TryAll` shown in the above example is its ability to ''pipeline'', or chain, operations,
 * catching and accumulating exceptions along the way. The `flatMap` and `map` combinators in the above example each
 * essentially pass off either their successfully completed value, wrapped in the `Success` type for it to be further
 * operated upon by the next combinator in the chain, or the exceptions wrapped in the `Failure` type usually to be
 * simply passed on down the chain. Combinators such as `rescue` and `recover` are designed to provide some type of
 * default behavior in the case of failure.
 *
 * ''Note'': only non-fatal exceptions are caught by the combinators on `TryAll` (see [[scala.util.control.NonFatal]]).
 * Serious system errors, on the other hand, will be thrown.
 *
 * ''Note:'': all TryAll combinators will catch exceptions and return failure unless otherwise specified in the
 * documentation.
 *
 * @author based on scala.util.Try
 * @since 2.10
 */
sealed abstract class TryAll[+T] {

  /**
   * @return list of exceptions if this is a `Failure`, `Nil` otherwise.
   */
  def exceptions : List[Throwable]

  /** Returns `true` if this is a `Failure`, `false` otherwise.
    */
  def isFailure: Boolean

  /** Returns `true` if this is a `Success`, `false` otherwise.
    */
  def isSuccess: Boolean

  /** Returns Some(value) if this is a `Success` or the given `default` argument if this is a `Failure`.
    *
    * ''Note:'': This will throw an exception if it is not a success and default throws an exception.
    */
  def getOrElse[U >: T](default: => U): U =
    if (isSuccess) get else default

  /** Returns this if it is a `Success` or the given `default` argument if this is a `Failure`.
    */
  def orElse[U >: T](default: => TryAll[U]): TryAll[U]
//    try if (isSuccess) this else default
//    catch {
//      case NonFatal(e) => Failure(e :: Nil)
//    }

  /** Returns the Some(value) from this `Success` or None if there was a `Failure`.
    */
  def get: T

  /**
   * Applies the given function `f` if this is a `Success`, otherwise returns `Unit` if this is a `Failure`.
   *
   * ''Note:'' If `f` throws, then this method may throw an exception.
   */
  def foreach[U](f: T => U): Unit

  /**
   * Returns the given function applied to the value from this `Success` or returns this if this is a `Failure`.
   */
  def flatMap[U](f: Option[T] => TryAll[U]): TryAll[U]

  /**
   * Maps the given function to the value from this `Success` or returns this if this is a `Failure`.
   */
  def map[U](f: Option[T] => U): TryAll[U]

  /**
   * Converts this to a `Failure` if the predicate is not satisfied.
   */
  def filter(p: T => Boolean): TryAll[T]

  /**
   * Applies the given function `f` if this is a `Failure`, otherwise returns this if this is a `Success`.
   * This is like `flatMap` for the exception.
   */
  def recoverWith[U >: T](f: PartialFunction[List[Throwable], TryAll[U]]): TryAll[U]

  /**
   * Applies the given function `f` if this is a `Failure`, otherwise returns this if this is a `Success`.
   * This is like map for the exception.
   */
  def recover[U >: T](f: PartialFunction[List[Throwable], U]): TryAll[U]

  /** Returns the Some(value) from this `Success` or None if there was a `Failure`.
   */
  def toOption: Option[T]

  /**
   * Transforms a nested `TryAll`, ie, a `TryAll` of type `TryAll[TryAll[T]]`,
   * into an un-nested `TryAll`, ie, a `TryAll` of type `TryAll[T]`.
   */
  def flatten[U](implicit ev: T <:< TryAll[U]): TryAll[U]

  /**
   * Completes this `TryAll` with the exceptions wrapped in a `Success`. The exceptions are either the exception that
   * the `TryAll` failed with (if a `Failure`) or an `UnsupportedOperationException`.
   */
  def failed: TryAll[List[Throwable]]

  /** Completes this `TryAll` by applying the function `f` to this if this is of type `Failure`, or conversely, by
    * applying  `s` if this is a `Success`.
    */
  def transform[U](s: T => TryAll[U], f: List[Throwable] => TryAll[U]): TryAll[U] =
    try this match {
      case Success(v) => s(v)
      case Failure(es) => f(es)
    } catch {
      case NonFatal(e) => Failure(e :: Nil)
    }

  /** Returns the scala.util.Success(value) from this is `Success` or scala.util.Failure(Exception(exceptions)) if there was a `Failure`.
    */
  def toTry : Try[T]

  /** No operation if this is `Success` or throws Exceptions(exceptions) if this is a `Failure`.
    */
  def throwIfFailure : Unit
}

object TryAll {
  object util {
    // Generated! see pattern generator below
    def tryAll[A,B,ZZ](fa: => A, fb: => B)(f: (Option[A], Option[B]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb)) yield f(oa,ob)
    def tryAll[A,B,C,ZZ](fa: => A, fb: => B, fc: => C)(f: (Option[A], Option[B], Option[C]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc)) yield f(oa,ob,oc)
    def tryAll[A,B,C,D,ZZ](fa: => A, fb: => B, fc: => C, fd: => D)(f: (Option[A], Option[B], Option[C], Option[D]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd)) yield f(oa,ob,oc,od)
    def tryAll[A,B,C,D,E,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E)(f: (Option[A], Option[B], Option[C], Option[D], Option[E]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe)) yield f(oa,ob,oc,od,oe)
    def tryAll[A,B,C,D,E,F,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff)) yield f(oa,ob,oc,od,oe,of)
    def tryAll[A,B,C,D,E,F,G,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg)) yield f(oa,ob,oc,od,oe,of,og)
    def tryAll[A,B,C,D,E,F,G,H,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh)) yield f(oa,ob,oc,od,oe,of,og,oh)
    def tryAll[A,B,C,D,E,F,G,H,I,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi)) yield f(oa,ob,oc,od,oe,of,og,oh,oi)
    def tryAll[A,B,C,D,E,F,G,H,I,J,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I], Option[J]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi);oj <- TryAll(fj)) yield f(oa,ob,oc,od,oe,of,og,oh,oi,oj)
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I], Option[J], Option[K]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi);oj <- TryAll(fj);ok <- TryAll(fk)) yield f(oa,ob,oc,od,oe,of,og,oh,oi,oj,ok)
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I], Option[J], Option[K], Option[L]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi);oj <- TryAll(fj);ok <- TryAll(fk);ol <- TryAll(fl)) yield f(oa,ob,oc,od,oe,of,og,oh,oi,oj,ok,ol)
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I], Option[J], Option[K], Option[L], Option[M]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi);oj <- TryAll(fj);ok <- TryAll(fk);ol <- TryAll(fl);om <- TryAll(fm)) yield f(oa,ob,oc,od,oe,of,og,oh,oi,oj,ok,ol,om)
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I], Option[J], Option[K], Option[L], Option[M], Option[N]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi);oj <- TryAll(fj);ok <- TryAll(fk);ol <- TryAll(fl);om <- TryAll(fm);on <- TryAll(fn)) yield f(oa,ob,oc,od,oe,of,og,oh,oi,oj,ok,ol,om,on)
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I], Option[J], Option[K], Option[L], Option[M], Option[N], Option[O]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi);oj <- TryAll(fj);ok <- TryAll(fk);ol <- TryAll(fl);om <- TryAll(fm);on <- TryAll(fn);oo <- TryAll(fo)) yield f(oa,ob,oc,od,oe,of,og,oh,oi,oj,ok,ol,om,on,oo)
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O, fp: => P)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I], Option[J], Option[K], Option[L], Option[M], Option[N], Option[O], Option[P]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi);oj <- TryAll(fj);ok <- TryAll(fk);ol <- TryAll(fl);om <- TryAll(fm);on <- TryAll(fn);oo <- TryAll(fo);op <- TryAll(fp)) yield f(oa,ob,oc,od,oe,of,og,oh,oi,oj,ok,ol,om,on,oo,op)
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O, fp: => P, fq: => Q)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I], Option[J], Option[K], Option[L], Option[M], Option[N], Option[O], Option[P], Option[Q]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi);oj <- TryAll(fj);ok <- TryAll(fk);ol <- TryAll(fl);om <- TryAll(fm);on <- TryAll(fn);oo <- TryAll(fo);op <- TryAll(fp);oq <- TryAll(fq)) yield f(oa,ob,oc,od,oe,of,og,oh,oi,oj,ok,ol,om,on,oo,op,oq)
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O, fp: => P, fq: => Q, fr: => R)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I], Option[J], Option[K], Option[L], Option[M], Option[N], Option[O], Option[P], Option[Q], Option[R]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi);oj <- TryAll(fj);ok <- TryAll(fk);ol <- TryAll(fl);om <- TryAll(fm);on <- TryAll(fn);oo <- TryAll(fo);op <- TryAll(fp);oq <- TryAll(fq);or <- TryAll(fr)) yield f(oa,ob,oc,od,oe,of,og,oh,oi,oj,ok,ol,om,on,oo,op,oq,or)
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O, fp: => P, fq: => Q, fr: => R, fs: => S)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I], Option[J], Option[K], Option[L], Option[M], Option[N], Option[O], Option[P], Option[Q], Option[R], Option[S]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi);oj <- TryAll(fj);ok <- TryAll(fk);ol <- TryAll(fl);om <- TryAll(fm);on <- TryAll(fn);oo <- TryAll(fo);op <- TryAll(fp);oq <- TryAll(fq);or <- TryAll(fr);os <- TryAll(fs)) yield f(oa,ob,oc,od,oe,of,og,oh,oi,oj,ok,ol,om,on,oo,op,oq,or,os)
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O, fp: => P, fq: => Q, fr: => R, fs: => S, ft: => T)(f: (Option[A], Option[B], Option[C], Option[D], Option[E], Option[F], Option[G], Option[H], Option[I], Option[J], Option[K], Option[L], Option[M], Option[N], Option[O], Option[P], Option[Q], Option[R], Option[S], Option[T]) => ZZ) : TryAll[ZZ] = for(oa <- TryAll(fa);ob <- TryAll(fb);oc <- TryAll(fc);od <- TryAll(fd);oe <- TryAll(fe);of <- TryAll(ff);og <- TryAll(fg);oh <- TryAll(fh);oi <- TryAll(fi);oj <- TryAll(fj);ok <- TryAll(fk);ol <- TryAll(fl);om <- TryAll(fm);on <- TryAll(fn);oo <- TryAll(fo);op <- TryAll(fp);oq <- TryAll(fq);or <- TryAll(fr);os <- TryAll(fs);ot <- TryAll(ft)) yield f(oa,ob,oc,od,oe,of,og,oh,oi,oj,ok,ol,om,on,oo,op,oq,or,os,ot)

    def tryAll_gen(min: Int, max: Int) = {
      (min to max).map { i =>
        val uc = ('A' to 'Z')
        val lc = ('a' to 'z')
        val genParms = uc.take(i).mkString(",") + ",ZZ" // A,B,C,...,ZZ
        val f1Parms = (0 to i-1).map(j =>
          s"f${lc(j)}: => ${uc(j)}"
        ).mkString(", ") // fa: => A, fb => B, ...
        val f2Parms = (0 to i-1).map(j =>
          s"Option[${uc(j)}]"
        ).mkString(", ") // Option[A],Option[B],...
        val forParms = (0 to i-1).map(j =>
          s"o${lc(j)} <- TryAll(f${lc(j)})"
        ).mkString(";") // oa <- Try(fa);ob <- TryAll(fb),...
        val invParms = (0 to i-1).map(j =>
          s"o${lc(j)}"
        ).mkString(",") // oa,ob,...
        s"def tryAll[$genParms]($f1Parms)(f: ($f2Parms) => ZZ) : TryAll[ZZ] = for($forParms) yield f($invParms)"
      }
    }
  }

  /** Constructs a `TryAll` using the by-name parameter.  This
    * method will ensure any non-fatal exception is caught and a
    * `Failure` object is returned.
    */
  def apply[T](r: => T): TryAll[T] =
    try Success(r) catch {
      case NonFatal(e) => Failure(e :: Nil)
    }

}

final case class Failure[+T](_exceptions: List[Throwable]) extends TryAll[T] {
  lazy val exceptions = _exceptions.map(_ match {
    case Exceptions(exceptions) => exceptions
    case exception => exception :: Nil
  }).flatten
  def isFailure: Boolean = true
  def isSuccess: Boolean = false
  def recoverWith[U >: T](f: PartialFunction[List[Throwable], TryAll[U]]): TryAll[U] =
    try {
      if (f isDefinedAt exceptions) f(exceptions) else this
    } catch {
      case NonFatal(e) => Failure(e :: Nil)
    }
  def get: T = throw Exceptions(exceptions)
  def flatMap[U](f: Option[T] => TryAll[U]): TryAll[U] = Failure(exceptions ::: f(None).exceptions)
  def flatten[U](implicit ev: T <:< TryAll[U]): TryAll[U] = this.asInstanceOf[TryAll[U]]
  def foreach[U](f: T => U): Unit = { }
  def map[U](f: Option[T] => U): TryAll[U] = Failure(exceptions ::: TryAll(f(None)).exceptions)
  def filter(p: T => Boolean): TryAll[T] = this
  def recover[U >: T](rescueException: PartialFunction[List[Throwable], U]): TryAll[U] =
    try {
      if (rescueException isDefinedAt exceptions) {
        TryAll(rescueException(exceptions))
      } else this
    } catch {
      case NonFatal(e) => Failure(e :: exceptions)
    }
  def failed: TryAll[List[Throwable]] = Success(exceptions)
  def toOption = None
  def toTry = scala.util.Failure(Exceptions(exceptions))
  def orElse[U >: T](default: => TryAll[U]): TryAll[U] = default
  def throwIfFailure : Unit = throw Exceptions(exceptions)
}


final case class Success[+T](value: T) extends TryAll[T] {
  def exceptions = Nil
  def isFailure: Boolean = false
  def isSuccess: Boolean = true
  def recoverWith[U >: T](f: PartialFunction[List[Throwable], TryAll[U]]): TryAll[U] = this
  def get = value
  def flatMap[U](f: Option[T] => TryAll[U]): TryAll[U] =
    try f(Some(value))
    catch {
      case NonFatal(e) => Failure(e :: Nil)
    }
  def flatten[U](implicit ev: T <:< TryAll[U]): TryAll[U] = value
  def foreach[U](f: T => U): Unit = f(value)
  def map[U](f: Option[T] => U): TryAll[U] = TryAll[U](f(Some(value)))
  def filter(p: T => Boolean): TryAll[T] = {
    try {
      if (p(value)) this
      else throw new NoSuchElementException("Predicate does not hold for " + value)
    } catch {
      case NonFatal(e) => Failure(e :: Nil)
    }
  }
  def recover[U >: T](rescueException: PartialFunction[List[Throwable], U]): TryAll[U] = this
  def failed: TryAll[List[Throwable]] = Failure(new UnsupportedOperationException("Success.failed") :: Nil)
  def toOption = Some(value)
  def toTry = scala.util.Success(value)
  def orElse[U >: T](default: => TryAll[U]): TryAll[U] = this
  def throwIfFailure : Unit = { }
}