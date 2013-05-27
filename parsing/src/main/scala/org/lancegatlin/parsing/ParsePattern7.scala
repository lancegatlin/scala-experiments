package org.lancegatlin.parsing

import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.xml.NodeSeq
import play.api.libs.json.Json.JsValueWrapper
import scala.util.Try

// Use play's JSON as the intermediate value for parsing and validation
// Doc: http://mandubian.com/2012/09/08/unveiling-play-2-dot-1-json-api-part1-jspath-reads-combinators/
// Pros:
// 1) Very DRY - validation code occurs in one spot and to create a Person the validation code must be run first
// 2) play JSON api 2.2 has added a very nice error accumulation monad JsResult. All errors are returned instead of just the first.
// 3)
// Cons:
// 1) JSON must be utilized even when constructing a Person instance in memory (this could be avoided with a more
// complex design that makes the validation independent of JSON)
// 2) No way to tie where in the original source the error occurred to the error message returned
// 3) All validation errors will be reported as play JSON errors which might be unexpected
object ParsePattern7 {
  // Prevent construction except by object Person.fromJson
  case class Person private(
    firstName: String,
    middleName: String,
    lastName: String,
    age: Int
  )

  object Person {
    // Note: Play can auto-generate format for most basic case classes using macros
//    implicit val fmt = Json.format[Person]

    implicit val fmt : Format[Person] = (
      (__ \ "firstName").format[String] and // Could do further validation here if we wanted
      (__ \ "middleName").format[String] and
      (__ \ "lastName").format[String] and
      (__ \ "age").format[Int]
    )(apply _, unlift(unapply))

    // Only way to create a Person
    def fromJson(jv: JsValue) = fmt.reads(jv)
  }

  def parsePerson(xml: NodeSeq) : JsResult[Person] = {
    def extract[T](fieldName : String, f: String => JsValueWrapper) =
      (xml\fieldName).headOption.map(node => (fieldName -> f(node.text)))

    val fields : List[(String, JsValueWrapper)]= {
        extract("firstName", { s: String => Json.toJson(s) }) ::
        extract("middleName", { s: String => Json.toJson(s) }) ::
        extract("lastName", { s: String => Json.toJson(s) }) ::
        extract("age", { s: String => Json.toJson(Try(s.toInt).toOption)}) ::
        Nil
    }.flatten

    Person.fromJson(Json.obj(fields:_*))
  }
}
