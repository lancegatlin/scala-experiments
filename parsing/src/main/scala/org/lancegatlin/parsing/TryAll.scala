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

  /**
   * Merge exceptions from this exceptions into that
   */
  def merge[U](that: TryAll[U]) : TryAll[U]
}

object TryAll {
  object util {
    // Generated! see pattern generator below
    def tryAll[A,B,ZZ](fa: => A, fb: => B)(f: (TryAll[A], TryAll[B]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb); ta merge tb merge TryAll(f(ta,tb)) }
    def tryAll[A,B,C,ZZ](fa: => A, fb: => B, fc: => C)(f: (TryAll[A], TryAll[B], TryAll[C]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc); ta merge tb merge tc merge TryAll(f(ta,tb,tc)) }
    def tryAll[A,B,C,D,ZZ](fa: => A, fb: => B, fc: => C, fd: => D)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd); ta merge tb merge tc merge td merge TryAll(f(ta,tb,tc,td)) }
    def tryAll[A,B,C,D,E,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe); ta merge tb merge tc merge td merge te merge TryAll(f(ta,tb,tc,td,te)) }
    def tryAll[A,B,C,D,E,F,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff); ta merge tb merge tc merge td merge te merge tf merge TryAll(f(ta,tb,tc,td,te,tf)) }
    def tryAll[A,B,C,D,E,F,G,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg); ta merge tb merge tc merge td merge te merge tf merge tg merge TryAll(f(ta,tb,tc,td,te,tf,tg)) }
    def tryAll[A,B,C,D,E,F,G,H,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge TryAll(f(ta,tb,tc,td,te,tf,tg,th)) }
    def tryAll[A,B,C,D,E,F,G,H,I,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti)) }
    def tryAll[A,B,C,D,E,F,G,H,I,J,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I], TryAll[J]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi);val tj = TryAll(fj); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge tj merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti,tj)) }
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I], TryAll[J], TryAll[K]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi);val tj = TryAll(fj);val tk = TryAll(fk); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge tj merge tk merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti,tj,tk)) }
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I], TryAll[J], TryAll[K], TryAll[L]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi);val tj = TryAll(fj);val tk = TryAll(fk);val tl = TryAll(fl); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge tj merge tk merge tl merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti,tj,tk,tl)) }
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I], TryAll[J], TryAll[K], TryAll[L], TryAll[M]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi);val tj = TryAll(fj);val tk = TryAll(fk);val tl = TryAll(fl);val tm = TryAll(fm); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge tj merge tk merge tl merge tm merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti,tj,tk,tl,tm)) }
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I], TryAll[J], TryAll[K], TryAll[L], TryAll[M], TryAll[N]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi);val tj = TryAll(fj);val tk = TryAll(fk);val tl = TryAll(fl);val tm = TryAll(fm);val tn = TryAll(fn); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge tj merge tk merge tl merge tm merge tn merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti,tj,tk,tl,tm,tn)) }
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I], TryAll[J], TryAll[K], TryAll[L], TryAll[M], TryAll[N], TryAll[O]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi);val tj = TryAll(fj);val tk = TryAll(fk);val tl = TryAll(fl);val tm = TryAll(fm);val tn = TryAll(fn);val to = TryAll(fo); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge tj merge tk merge tl merge tm merge tn merge to merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti,tj,tk,tl,tm,tn,to)) }
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O, fp: => P)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I], TryAll[J], TryAll[K], TryAll[L], TryAll[M], TryAll[N], TryAll[O], TryAll[P]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi);val tj = TryAll(fj);val tk = TryAll(fk);val tl = TryAll(fl);val tm = TryAll(fm);val tn = TryAll(fn);val to = TryAll(fo);val tp = TryAll(fp); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge tj merge tk merge tl merge tm merge tn merge to merge tp merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti,tj,tk,tl,tm,tn,to,tp)) }
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O, fp: => P, fq: => Q)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I], TryAll[J], TryAll[K], TryAll[L], TryAll[M], TryAll[N], TryAll[O], TryAll[P], TryAll[Q]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi);val tj = TryAll(fj);val tk = TryAll(fk);val tl = TryAll(fl);val tm = TryAll(fm);val tn = TryAll(fn);val to = TryAll(fo);val tp = TryAll(fp);val tq = TryAll(fq); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge tj merge tk merge tl merge tm merge tn merge to merge tp merge tq merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti,tj,tk,tl,tm,tn,to,tp,tq)) }
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O, fp: => P, fq: => Q, fr: => R)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I], TryAll[J], TryAll[K], TryAll[L], TryAll[M], TryAll[N], TryAll[O], TryAll[P], TryAll[Q], TryAll[R]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi);val tj = TryAll(fj);val tk = TryAll(fk);val tl = TryAll(fl);val tm = TryAll(fm);val tn = TryAll(fn);val to = TryAll(fo);val tp = TryAll(fp);val tq = TryAll(fq);val tr = TryAll(fr); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge tj merge tk merge tl merge tm merge tn merge to merge tp merge tq merge tr merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti,tj,tk,tl,tm,tn,to,tp,tq,tr)) }
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O, fp: => P, fq: => Q, fr: => R, fs: => S)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I], TryAll[J], TryAll[K], TryAll[L], TryAll[M], TryAll[N], TryAll[O], TryAll[P], TryAll[Q], TryAll[R], TryAll[S]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi);val tj = TryAll(fj);val tk = TryAll(fk);val tl = TryAll(fl);val tm = TryAll(fm);val tn = TryAll(fn);val to = TryAll(fo);val tp = TryAll(fp);val tq = TryAll(fq);val tr = TryAll(fr);val ts = TryAll(fs); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge tj merge tk merge tl merge tm merge tn merge to merge tp merge tq merge tr merge ts merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti,tj,tk,tl,tm,tn,to,tp,tq,tr,ts)) }
    def tryAll[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,ZZ](fa: => A, fb: => B, fc: => C, fd: => D, fe: => E, ff: => F, fg: => G, fh: => H, fi: => I, fj: => J, fk: => K, fl: => L, fm: => M, fn: => N, fo: => O, fp: => P, fq: => Q, fr: => R, fs: => S, ft: => T)(f: (TryAll[A], TryAll[B], TryAll[C], TryAll[D], TryAll[E], TryAll[F], TryAll[G], TryAll[H], TryAll[I], TryAll[J], TryAll[K], TryAll[L], TryAll[M], TryAll[N], TryAll[O], TryAll[P], TryAll[Q], TryAll[R], TryAll[S], TryAll[T]) => ZZ) : TryAll[ZZ] = { val ta = TryAll(fa);val tb = TryAll(fb);val tc = TryAll(fc);val td = TryAll(fd);val te = TryAll(fe);val tf = TryAll(ff);val tg = TryAll(fg);val th = TryAll(fh);val ti = TryAll(fi);val tj = TryAll(fj);val tk = TryAll(fk);val tl = TryAll(fl);val tm = TryAll(fm);val tn = TryAll(fn);val to = TryAll(fo);val tp = TryAll(fp);val tq = TryAll(fq);val tr = TryAll(fr);val ts = TryAll(fs);val tt = TryAll(ft); ta merge tb merge tc merge td merge te merge tf merge tg merge th merge ti merge tj merge tk merge tl merge tm merge tn merge to merge tp merge tq merge tr merge ts merge tt merge TryAll(f(ta,tb,tc,td,te,tf,tg,th,ti,tj,tk,tl,tm,tn,to,tp,tq,tr,ts,tt)) }

    def tryAll_gen(min: Int, max: Int) = {
      (min to max).map { i =>
        val uc = ('A' to 'Z')
        val lc = ('a' to 'z')
        val genParms = uc.take(i).mkString(",") + ",ZZ" // A,B,C,...,ZZ
        val f1Parms = (0 to i-1).map(j =>
          s"f${lc(j)}: => ${uc(j)}"
        ).mkString(", ") // fa: => A, fb => B, ...
        val f2Parms = (0 to i-1).map(j =>
          s"TryAll[${uc(j)}]"
        ).mkString(", ") // TryAll[A], TryAll[B], ...
        val vals = (0 to i-1).map(j =>
          s"val t${lc(j)} = TryAll(f${lc(j)})"
        ).mkString(";") // val ta = Try(fa);val tb = TryAll(fb); ...
        val merge = (0 to i-1).map(j =>
          s"t${lc(j)}"
        ).mkString(" merge ") // ta merge tb merge ...
        val invParms = (0 to i-1).map(j =>
          s"t${lc(j)}"
        ).mkString(",") // ta,tb, ...
        s"def tryAll[$genParms]($f1Parms)(f: ($f2Parms) => ZZ) : TryAll[ZZ] = { $vals; $merge merge TryAll(f($invParms)) }"
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
  def merge[U](that: TryAll[U]) : TryAll[U] = Failure(this.exceptions ++ that.exceptions)
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
  def merge[U](that: TryAll[U]) : TryAll[U] = that
}