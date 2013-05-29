package org.lancegatlin.parsing

import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.xml._
import scala.util._
import scala.Predef._
import play.api.data.validation.ValidationError

// Parsing steps:
// 1) Field extraction - handled by serialization code (XML, JSON, CSV, etc)
// 2) Convert string to field type - handled by serialization code
// 3) Check for missing fields
// 4) Validate fields that are present1
object ParsePattern9 {
  import TryAll.util._

  case class HandleValidateExceptionFormat[A](fmt: Format[A]) extends Format[A] {
    private[this] def handler: PartialFunction[Throwable,JsError] = {
      case ve : ValidateException => {
        // Associate the path to the field with the error
        val key = JsPath(KeyPathNode(ve.field.toString) :: Nil)
        val errors = ValidationError(ve.getMessage) :: Nil
        JsError((key,errors) :: Nil)
      }
      case e : Exception => JsError(ValidationError(e.getMessage))
    }

    def reads(jv: JsValue): JsResult[A] = {
      try {
        fmt.reads(jv)
      } catch {
        case Exceptions(list) => list.map(handler(_)).reduce(_ ++ _)
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
    import ValidateException.util._
    validateAll(
      isSet('firstName, firstName),
      isSet('middleName, middleName),
      isSet('lastName, lastName),
      range('age, age)(0 ,  150)
    ).throwIfFailure
  }

  object Person {
    implicit val json = HandleValidateExceptionFormat[Person](Json.format[Person])

    // A Person that passes validation whose fields can be used for partial validation
    val valid = Person("dummy value", "dummy value", "dummy value", 35)
  }

  def parsePerson(xml: NodeSeq) : Try[Person] = {
    def extract(fieldName : String) : String = (xml \ fieldName).headOption.map(_.text).getOrElse(throw MissingValueException(fieldName))

    def extractMap[T](fieldName : String, f: String => T) : T = {
      val v = extract(fieldName)
      try {
        f(v)
      } catch {
        case e : Exception => throw InvalidValueException(fieldName, v)
      }
    }

    tryAll(
      extract("firstName"),
      extract("middleName"),
      extract("lastName"),
      extractMap("age", { _.toInt })
    ) { (oa,ob,oc,od) =>
      Person(
        // Note: getOrElse dummy values always pass validation
        // If a None makes it to this point then the Person created here will be discarded and an exception list
        // returned. However, to get the full set of possible validation errors we need to go ahead and try the values
        // that were successfully extracted against the Person constructor.

        oa.getOrElse(Person.valid.firstName),
        ob.getOrElse(Person.valid.middleName),
        oc.getOrElse(Person.valid.lastName),
        od.getOrElse(Person.valid.age)
      )
    }.toTry
  }
}

