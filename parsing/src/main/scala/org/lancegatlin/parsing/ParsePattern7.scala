package org.lancegatlin.parsing

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.util._
import play.api.libs.json.Reads._
import scala.xml.NodeSeq
import play.api.libs.json.Json.JsValueWrapper
import scala.util.Try

// Use play's JSON as the intermediate value for parsing and validation
// Doc: http://mandubian.com/2012/09/08/unveiling-play-2-dot-1-json-api-part1-jspath-reads-combinators/
// Pros:
// 1) Very DRY - validation code occurs in one spot. To create a Person the validation code must run first.
// 2) play JSON api 2.2 has added a very nice error accumulation framework with monad JsResult. It returns either
// JsSuccess with the Person or JsError which contains all errors that occurred in the JSON
// 3) Super simple - minimal boilerplate required
// 4) "Free" JSON serialization
// 5) Compile-time errors that result when using the play JSON Reads/Writes/Format DSL are very difficult to troubleshoot
// Cons:
// 1) JSON must be utilized even when constructing a Person instance in memory (this could be avoided with a more
// complex design that makes the validation independent of JSON but probably not worth the trade off)
// 2) No way to tie where in the original source the error occurred (e.g. xml,csv) to the error message returned
// 3) All validation errors will be reported as play JSON errors, which might be unexpected especially when constructing
// an in-memory Person
object ParsePattern7 {
  // Prevent construction except by object Person.fromJson
  case class Person private(
    firstName: String,
    middleName: String,
    lastName: String,
    age: Int
  )

  object Person {
    // Note: can't call this apply since it is not possible to override the default apply for case classes
    // This is provided as an example, but probably better to just omit this method and let user build their own JSON
    def create(firstName: String, middleName: String, lastName: String, age: Int) : Person = {
      // Make a JSON object out of parameters then parse it to ensure parameters get validated
      Person.fromJson(Json.obj(
        "firstName" -> firstName,
        "middleName" -> middleName,
        "lastName" -> lastName,
        "age" -> age
      )).get // throws if an error occurred
    }

    // Note: Play can auto-generate Format,Reads and Writes for most basic case classes using macros
//    implicit val fmt = Json.format[Person]

    implicit val fmt : Format[Person] = (
      (__ \ "firstName").format[String] and // Could do further validation here if we wanted
      (__ \ "middleName").format[String] and
      (__ \ "lastName").format[String] and
      (__ \ "age").format[Int](max(150))
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
