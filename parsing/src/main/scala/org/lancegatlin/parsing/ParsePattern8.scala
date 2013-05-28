package org.lancegatlin.parsing

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.util._
import play.api.libs.json.Reads._
import scala.xml._
import scala.util._
import scala.collection.immutable.Seq
import play.api.libs.json.Json.JsValueWrapper
import play.api.data.validation.ValidationError
import scala.Predef._
import scala.util.Failure
import scala.Some
import play.api.data.validation.ValidationError
import scala.util.Success
import scala.util.control.NonFatal

object ParsePattern8 {

//  implicit class pimpMyFunction4[A,B,C,D,E](val me : Function4[A,B,C,D,E]) extends AnyVal {
//    def lift = { (oa: Option[A],ob:Option[B],oc:Option[C],od:Option[D]) =>
//      for(a <- oa;b <- ob;c <- oc;d <- od) yield me(a,b,c,d)
//    }
//  }

  case class Exceptions(xs: Seq[Throwable]) extends Exception {
    override def getMessage = toString
    override def toString = s"Exceptions(${xs.mkString(", ")})"
  }

//  case class TryWriter[A](failures: List[Throwable], opt: Option[A]) {
//    def map[B](f: Option[A] => Option[B]) : TryWriter[B] = TryWriter(failures, f(opt))
//    def flatMap[B](f: Option[A] => TryWriter[B]) : TryWriter[B] = {
//      val inner = f(opt)
//      TryWriter(failures ::: inner.failures, inner.opt)
//    }
//    def isSuccess = !hasFailures
//    def get : A = opt.get
//    def hasFailures = failures.length > 0
//    def throwIfFailure : Unit = if(hasFailures) throw Exceptions(failures)
//  }
//
//  object TryWriter {
//    def apply[A](f: => A) : TryWriter[A] = Try(f) match {
//      case Success(value) => TryWriter(Nil, Some(value))
//      case Failure(ex) => TryWriter(ex :: Nil, None)
//    }
//  }
//
//  def tryAll[A,B](fa: => A, fb: => B) : TryWriter[(A,B)] = for(oa <- TryWriter(fa);ob <- TryWriter(fb)) yield for(a <- oa;b <- ob) yield (a,b)
//  def tryAll[A,B,C](fa: => A, fb: => B, fc: => C) : TryWriter[(A,B,C)] = for(oa <- TryWriter(fa);ob <- TryWriter(fb);oc <- TryWriter(fc)) yield for(a <- oa;b <- ob;c <- oc) yield (a,b,c)
//  def tryAll[A,B,C,D](fa: => A, fb: => B, fc: => C,fd: => D) : TryWriter[(A,B,C,D)] = for(oa <- TryWriter(fa);ob <- TryWriter(fb);oc <- TryWriter(fc);od <- TryWriter(fd)) yield for(a <- oa;b <- ob;c <- oc;d <- od) yield (a,b,c,d)
//  // ...

  implicit class pimpMyListThrowable(val me: List[Throwable]) extends AnyVal {
    def hasFailures = me.length > 0
    def throwIfFailure : Unit = if(hasFailures) throw Exceptions(me)
  }

  def requireAll(f: => Unit) = try { f; Nil } catch { case NonFatal(e) => e :: Nil }
  def requireAll(fa: => Unit, fb: => Unit) : List[Throwable] = requireAll(fa) ::: requireAll(fb)
  def requireAll(fa: => Unit, fb: => Unit, fc: => Unit) : List[Throwable] = requireAll(fa) ::: requireAll(fb) ::: requireAll(fc)
  def requireAll(fa: => Unit, fb: => Unit, fc: => Unit, fd: => Unit) : List[Throwable] = requireAll(fa) ::: requireAll(fb) ::: requireAll(fc) ::: requireAll(fd)
  def requireAll(fa: => Unit, fb: => Unit, fc: => Unit, fd: => Unit, fe: => Unit) : List[Throwable] = requireAll(fa) ::: requireAll(fb) ::: requireAll(fc) ::: requireAll(fd) ::: requireAll(fe)


  case class Person(
    firstName: String,
    middleName: String,
    lastName: String,
    age: Int
  ) {
    import Person._
    (
      validate_firstName(firstName) :::
      validate_middleName(middleName) :::
      validate_lastName(lastName) :::
      validate_age(age)
    ).throwIfFailure
  }

  object Person {

    def validate_firstName(firstName: String) = requireAll(
      require(firstName.length > 0, "firstName must not be empty")
    )
    def validate_middleName(middleName: String) = requireAll(
      require(middleName.length > 0, "middleName must not be empty")
    )
    def validate_lastName(lastName: String) = requireAll(
      require(lastName.length > 0, "lastName must not be empty")
    )
    def validate_age(age: Int) = requireAll(
      require(age > 0, s"age $age must be greater than 0"),
      require(age < 150, s"age $age must be less than 150")      
    )
    
    def validate_json[A](validate: A => List[Throwable])(implicit a_reads: Reads[A]) = Reads[A] { jv =>
      a_reads.reads(jv).flatMap(a => validate(a) match {
        case Nil => JsSuccess(a)
        case list => JsError(ValidationError(Exceptions(list).toString))
      })
    }
    
    implicit val fmt : Format[Person] = (
        (__ \ "firstName").format[String](validate_json(validate_firstName)) and // Need to perform validation here since
        (__ \ "middleName").format[String](validate_json(validate_middleName)) and
        (__ \ "lastName").format[String](validate_json(validate_lastName)) and
        (__ \ "age").format[Int](validate_json(validate_age))
    )(apply _, unlift(unapply))

    // Only way to create a Person
    def fromJson(jv: JsValue) = fmt.reads(jv)
  }

  def parsePerson(xml: NodeSeq) : JsResult[Person] = {
    def extract[T](fieldName : String, f: String => JsValueWrapper) =
      (xml\fieldName).headOption.map(node => (fieldName -> f(node.text)))

    val fields : List[(String, JsValueWrapper)]= {
        extract("firstName", { s: String => Json.toJson(s) }) :: // Moving Json.toJson into extract[T] would require dealing with type erasure
        extract("middleName", { s: String => Json.toJson(s) }) ::
        extract("lastName", { s: String => Json.toJson(s) }) ::
        extract("age", { s: String => Json.toJson(Try(s.toInt).toOption)}) ::
        Nil
    }.flatten

    Person.fromJson(Json.obj(fields:_*))
  }
}
