package org.lancegatlin.parsing

import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.xml._
import scala.util._
import scala.Predef._
import play.api.data.validation.ValidationError


object ParsePattern9 {
  case class Exceptions(xs: List[Throwable], cause: Option[Throwable] = None) extends Exception(cause.orNull) {
    override lazy val toString = s"Exceptions(${xs.mkString(", ")})"
    override def getMessage = toString
  }

  case class TryWriter[A](failures: List[Throwable], opt: Option[A]) {
    def map[B](f: Option[A] => Option[B]) : TryWriter[B] = TryWriter(failures, f(opt))
    def flatMap[B](f: Option[A] => TryWriter[B]) : TryWriter[B] = {
      val inner = f(opt)
      TryWriter(failures ::: inner.failures, inner.opt)
    }
    def isSuccess = !hasFailures
    def get : A = opt.get
    def hasFailures = failures.length > 0
    def throwIfFailure : Unit = if(hasFailures) throw Exceptions(failures)
    def toTry : Try[A] = this match {
      case TryWriter(Nil, Some(value)) => Success(value)
      case _ => Failure(Exceptions(failures))
    }
    def toEither : Either[List[Throwable], A] = opt match {
      case Some(value) => Right(value)
      case None => Left(failures)
    }
  }

  object TryWriter {
    def apply[A](f: => A) : TryWriter[A] = Try(f) match {
      case Success(value) => TryWriter(Nil, Some(value))
      case Failure(ex : Throwable) => TryWriter(ex :: Nil, None)
    }
  }

  def tryAll[A,B,ZZ](fa: => A, fb: => B)(f: (A,B) => ZZ) : TryWriter[ZZ] = for(oa <- TryWriter(fa);ob <- TryWriter(fb)) yield for(a <- oa;b <- ob) yield f(a,b)
  def tryAll[A,B,C,ZZ](fa: => A, fb: => B, fc: => C)(f: (A,B,C) => ZZ) : TryWriter[ZZ] = for(oa <- TryWriter(fa);ob <- TryWriter(fb);oc <- TryWriter(fc)) yield for(a <- oa;b <- ob;c <- oc) yield f(a,b,c)
  def tryAll[A,B,C,D,ZZ](fa: => A, fb: => B, fc: => C, fd: => D)(f: (A,B,C,D) => ZZ) : TryWriter[ZZ] = for(oa <- TryWriter(fa);ob <- TryWriter(fb);oc <- TryWriter(fc);od <- TryWriter(fd)) yield for(a <- oa;b <- ob;c <- oc;d <- od) yield f(a,b,c,d)
  // ...

  
  implicit class pimpMyListException(val me: List[Exception]) extends AnyVal {
    def hasFailures = me.length > 0
    def throwIfFailure : Unit = if(hasFailures) throw Exceptions(me)
  }

  case class ValidateException(fieldName: String, value: String, message: String, cause: Option[Throwable] = None) extends Exception(cause.orNull) {
    override lazy val toString = s"$fieldName${if(value.length > 0) s"($value)" else ""} $message"
    override def getMessage = toString
  }
  
  def validate(fieldName: String, value: Any, test: Boolean, message : String) =
    if(test) Nil else ValidateException(fieldName, value.toString, message) :: Nil
  def validateAll(vs: List[ValidateException]*) : List[ValidateException] = vs.reduce(_ ::: _)

  case class HandleValidateFormat[A](fmt: Format[A]) extends Format[A] {
    private[this] def handler: PartialFunction[Throwable,JsError] = {
      case ve : ValidateException => {
        // Associate the path to the field with the error
        val key = JsPath(KeyPathNode(ve.fieldName.toString) :: Nil)
        val errors = ValidationError(ve.getMessage) :: Nil
        JsError((key,errors) :: Nil)
      }
      case e : Exception => JsError(ValidationError(e.getMessage))
    }

    def reads(jv: JsValue): JsResult[A] = {
      try {
        fmt.reads(jv)
      } catch {
        case Exceptions(list,_) => list.map(handler(_)).reduce(_ ++ _)
        case e : Exception => handler(e)
      }
    }

    def writes(a: A): JsValue = fmt.writes(a)
  }

  case class Person(
    firstName: String,
    middleName: String,
    lastName: String,
    age: Int
  ) {
    validateAll(
      validate("firstName",  firstName,     firstName.length > 0,   "must not be empty"),
      validate("middleName", middleName,    middleName.length > 0,  "must not be empty"),
      validate("lastName",   lastName,      lastName.length > 0,    "must not be empty"),
      validate("age",        age,           age > 0,                s"must be greater than 0"),
      validate("age",        age,           age < 150,              s"must be less than 150")
    ).throwIfFailure
  }

  object Person {
    implicit val json = HandleValidateFormat[Person](Json.format[Person])
//      (
//        (__ \ "firstName").format[String] and
//        (__ \ "middleName").format[String] and
//        (__ \ "lastName").format[String] and
//        (__ \ "age").format[Int]
//      )(apply _, unlift(unapply))

//    def fromJson(jv: JsValue) = json.reads(jv)
  }

  def parsePerson(xml: NodeSeq) : Try[Person] = {
    def extract(fieldName : String) : String =
      (xml \ fieldName).headOption.getOrElse(throw ValidateException(fieldName,"","missing")).text

    def extractMap[T](fieldName : String, f: String => T) : T = {
      val v = extract(fieldName)
      try {
        f(v)
      } catch {
        case e : Exception => throw ValidateException(fieldName,v,"is invalid", Some(e))
      }
    }

    tryAll(
      extract("firstName"),
      extract("middleName"),
      extract("lastName"),
      extractMap("age", { _.toInt })
    ) { (a,b,c,d) =>
      Try(Person(a,b,c,d))
    }.toTry.flatten
  }
}

