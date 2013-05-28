package org.lancegatlin.parsing

import scala.collection.immutable
import scala.xml._
import scala.util._
import java.lang.NumberFormatException

// This pattern breaks parsing into three phases:
// 1) Extraction: extract strings from input and map to key-value pairs (StringValues)
// 2) Validation: ensure all values are set and are the correct types. Returns ValidStringValues[A] if everythings valid
// 3) Convert: safely convert from ValidStringValues[A] to A
// Pros:
// 1) Allows re-use of validation and conversion code by writing different extractors (extract from XML, JSON, CSV,
// etc)
// 2) Code could be written to allow for automatically extracting, validating and converting case classes using
// reflection or macros
// Cons:
// 1) Much more verbose
// 2) No mechanism for validation messages to be associated with where they occur in the source
object ParsePattern6 {
  case class Person(
    firstName: String,
    middleName: String,
    lastName: String,
    age: Int
  )

  // Recursive key-value pair class (basically JSON)
  // Would prefer this if it were legal: type StringValues = Map[String, Either[String, StringValues]]
  // But this generates a cyclic type alias error
  trait StringValues extends immutable.Map[String, Either[String, StringValues]] {

    lazy val fields : Map[String, String] = this.map(tuple => tuple._2 match {
      case Left(sval) => Some(tuple._1,sval)
      case Right(_) => None
    }).flatten.toMap

    lazy val children : Map[String, StringValues] = this.map(tuple => tuple._2 match {
      case Right(child) => Some(tuple._1,child)
      case Left(_) => None
    }).flatten.toMap

    def getField(key : String) : Option[String] = repr.get(key) match {
      case Some(value) => value match {
        case Left(sval) => Some(sval)
        case Right(_) => None
      }
      case None => None
    }

    def getChild(key : String) : Option[StringValues] = repr.get(key) match {
      case Some(value) => value match {
        case Left(_) => None
        case Right(child) => Some(child)
      }
      case None => None
    }
  }

  object StringValues {
    def apply(m: Map[String, Either[String, StringValues]]) : StringValues = new StringValues {
      def get(key: String): Option[Either[String, StringValues]] = m.get(key)

      def iterator: Iterator[(String, Either[String, StringValues])] = m.iterator

      def -(key: String): Map[String, Either[String, StringValues]] = m.-(key)

      def +[B1 >: Either[String, StringValues]](kv: (String, B1)): Map[String, B1] = m.+(kv)
    }
  }

  implicit class pimpMyList(val me: List[(String, Either[String, StringValues])]) extends AnyVal {
    def toStringValues : StringValues = StringValues(me.toMap)
  }

  trait StringValuesExtractor[A,B] {
    def extract(source: B) : StringValues
  }

  implicit object PersonXmlExtractor extends StringValuesExtractor[Person, NodeSeq] {
    def extract(xml: NodeSeq) : StringValues = {
      (xml\"firstName").headOption.map(_.text).map(value => "firstName" -> Left(value)) ::
      (xml\"middleName").headOption.map(_.text).map(value => "middleName" -> Left(value)) ::
      (xml\"lastName").headOption.map(_.text).map(value => "lastName" -> Left(value)) ::
      (xml\"age").headOption.map(_.text).map(value => "age" -> Left(value)) ::
      Nil
    }.flatten.toStringValues
  }

  trait ValidStringValues[A] extends StringValues
  object ValidStringValues {
    def apply[A](svals : StringValues) : ValidStringValues[A] = new ValidStringValues[A] {
      def get(key: String): Option[Either[String, StringValues]] = svals.get(key)

      def iterator: Iterator[(String, Either[String, StringValues])] = svals.iterator

      def -(key: String): Map[String, Either[String, StringValues]] = svals.-(key)

      def +[B1 >: Either[String, StringValues]](kv: (String, B1)): Map[String, B1] = svals.+(kv)
    }
  }

  trait StringValuesValidator[A] {
    def validate(sval: StringValues) : Either[List[String], ValidStringValues[A]]
  }

  implicit object PersonValidator extends StringValuesValidator[Person] {
    def validate(svals: StringValues) : Either[List[String],ValidStringValues[Person]] = {
      def rule(name: String)(test: => Boolean) = if(test) Nil else List(name)

      rule("Person must have firstName")(
        test = { svals.getField("firstName").nonEmpty }
      ) ++
      rule("Person must have middleName")(
        test = { svals.getField("middleName").nonEmpty }
      ) ++
      rule("Person must have lastName")(
        test = { svals.getField("lastName").nonEmpty }
      ) ++
      rule("Person must have age")(
        test = { svals.getField("age").nonEmpty }
      ) ++
      rule("age must be an integer")(
        test = {
          svals.getField("age") match {
            case Some(value) => Try(value.toInt).isSuccess
            case None => false
          }
        }
      ) ++
      rule("age must be less than 150")(
        test = {
          svals.getField("age") match {
            case Some(s_age) => Try(s_age.toInt) match {
              case Success(age) => age < 150
              case _ => true // Don't fire the rule if its an Int
            }
            case None => true // Don't fire the rule if its missing
          }
        }
      ) match {
        case Nil => Right(ValidStringValues[Person](svals))
        case list  => Left(list)
      }
    }
  }

  trait StringValuesParser[A] {

    def parse[B](source : B)(implicit extractor: StringValuesExtractor[A,B], validator: StringValuesValidator[A]) : Either[List[String], A] = {
      val raw_svals = extractor.extract(source)
      val issues_or_valid_svals = validator.validate(raw_svals)
      issues_or_valid_svals match {
        case Right(valid_svals) => Right(parse(valid_svals))
        case Left(issues) => Left(issues)
      }
    }

    def parse(svals : ValidStringValues[A]) : A
  }

  implicit object PersonParser extends StringValuesParser[Person] {
    def parse(svals: ValidStringValues[Person]) = Person(
      svals.fields("firstName"),
      svals.fields("middleName"),
      svals.fields("lastName"),
      svals.fields("age").toInt
    )
  }
}
