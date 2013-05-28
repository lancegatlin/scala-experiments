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
      case Failure(ex) => TryWriter(ex :: Nil, None)
    }
  }

  def tryAll[A,B,ZZ](fa: => A, fb: => B)(f: (A,B) => ZZ) : TryWriter[ZZ] = for(oa <- TryWriter(fa);ob <- TryWriter(fb)) yield for(a <- oa;b <- ob) yield f(a,b)
  def tryAll[A,B,C,ZZ](fa: => A, fb: => B, fc: => C)(f: (A,B,C) => ZZ) : TryWriter[ZZ] = for(oa <- TryWriter(fa);ob <- TryWriter(fb);oc <- TryWriter(fc)) yield for(a <- oa;b <- ob;c <- oc) yield f(a,b,c)
  def tryAll[A,B,C,D,ZZ](fa: => A, fb: => B, fc: => C, fd: => D)(f: (A,B,C,D) => ZZ) : TryWriter[ZZ] = for(oa <- TryWriter(fa);ob <- TryWriter(fb);oc <- TryWriter(fc);od <- TryWriter(fd)) yield for(a <- oa;b <- ob;c <- oc;d <- od) yield f(a,b,c,d)
  // ...

  
  implicit class pimpMyListException(val me: List[Throwable]) extends AnyVal {
    def hasFailures = me.length > 0
    def throwIfFailure : Unit = if(hasFailures) throw Exceptions(me)
  }

  case class ValidateException(field: (Symbol,Any), message: String, cause: Option[Throwable] = None) extends Exception(cause.orNull) {
    def fieldName = field._1.name
    def value = field._2
    def valueStr = if(value != null && value.toString.length > 0) s"(${value.toString})" else ""
    override lazy val toString = s"$fieldName$valueStr $message"
    override def getMessage = toString
  }
  
  def validate(field: (Symbol, Any), test: Boolean, message : String) =
    if(test) Nil else ValidateException(field, message) :: Nil
  object Validate {

    implicit class pimpMySymbolTuple2[A](val field: (Symbol, A)) extends AnyVal {
      def fieldName = field._1
      def value = field._2
    }

    def isSet(field: (Symbol,String)) = validate(field,field.value != null && field.value.length > 0, "must be set")

    def min(field: (Symbol,Int))(min: Int) = validate(field, field.value > min, s"must be greater than $min")
    def max(field: (Symbol,Int))(max: Int) = validate(field, field.value < max, s"must be less than $max")
    def rng(field: (Symbol,Int))(min: Int, max: Int) = validate(field, min < field.value && field.value < max, s"must be greater than $min and less than $max")

  }
  def validateAll(vs: List[ValidateException]*) : List[ValidateException] = vs.reduce(_ ::: _)

  case class HandleValidateExceptionFormat[A](fmt: Format[A]) extends Format[A] {
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
    import Validate._
    validateAll(
      isSet('firstName -> firstName),
      isSet('middleName -> middleName),
      isSet('lastName -> lastName),
      rng('age -> age)(0 ,  150)
    ).throwIfFailure
  }

  object Person {
    implicit val json = HandleValidateExceptionFormat[Person](Json.format[Person])
//      (
//        (__ \ "firstName").format[String] and
//        (__ \ "middleName").format[String] and
//        (__ \ "lastName").format[String] and
//        (__ \ "age").format[Int]
//      )(apply _, unlift(unapply))

//    def fromJson(jv: JsValue) = json.reads(jv)
  }

  def parsePerson(xml: NodeSeq) : Try[Person] = {
    def extract(fieldName : String) : String = (xml \ fieldName).headOption.map(_.text).getOrElse(throw new ValidateException(Symbol(fieldName) -> null,"must be set"))

    def extractMap[T](fieldName : String, f: String => T) : T = {
      val v = extract(fieldName)
      try {
        f(v)
      } catch {
        case e : Exception => throw ValidateException(Symbol(fieldName) -> v,"is invalid", Some(e))
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

