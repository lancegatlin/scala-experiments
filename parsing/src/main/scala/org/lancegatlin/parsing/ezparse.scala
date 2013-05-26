package org.lancegatlin.parsing

import scala.language.postfixOps
import scala.xml._
import scala.util.Either.RightProjection
import scala.util.Try
import scala.Function4
import scala.collection.immutable

case class Person(
firstName: String,
middleName: String,
lastName: String,
age: Int
)

object MyApp extends App {
// TODO: Move this to separate experiment don't complicate this example with custom XML dsl
//  implicit class pimpMySeq[T](val me: Seq[T]) extends AnyVal {
//    def get(index : Int) : Option[T] = if(index < me.length) Some(me(index)) else None
//  }

//  implicit class pimpMyNodeSeq(val me: NodeSeq) extends AnyVal {
//    def x(elementName: String) = me \\ elementName
//    def \(index: Int) = me.get(index)
//    def x(index : Int) = me.get(index)
//  }
//
//  def text() : Node => String = { _.text }
//  def node() : Node => NodeSeq = { _.child }
//
//  implicit class pimpMyOptionNode(val me: Option[Node]) extends AnyVal {
//    def \[T](f: Node => T) = me.repr(f(_))
//  }

  // Parser that parses result OR returns None - no way to tell why it failed
  object ParsePattern1 {

    def parsePerson(xml: NodeSeq) : Option[Person] =
      for {
        // 1) Get a NodeSeq of all "firstName" elements
        // 2) Get the first element if it exists
        // 3) Map the element to its string content
        // 4) Extract the value from option
        firstName <- (xml\"firstName").headOption.map(_.text)

        // If firstName is None then Option.flatMap will ensure that execution stops above and None is returned for Option[Person]
        middleName <- (xml\"middleName").headOption.map(_.text)

        lastName <- (xml\"lastName").headOption.map(_.text)
        s_age <- (xml\"age").headOption.map(_.text)

        // If the age element is missing, execution will not reach this point
        // Using Try here to suppress the NumberFormatException
        age <- Try(s_age.toInt).toOption
      } yield
        Person(firstName,middleName,lastName,age)

  }

  // Parser that parses result OR provides an error message on the first error encountered
  object ParsePattern2 {

    // Syntatic sugar to return an error message if Option is None
    implicit class pimpMyOption[T](val me: Option[T]) extends AnyVal {
      def orErr(err: String) : RightProjection[String,T] = me match {
        case Some(value) => Right(value).right
        case None => Left(err).right
      }
    }

    def parsePerson(xml: NodeSeq) : Either[String, Person] =
      for {

        // Same as above except now we are using Either[String, String]
        // onErr will ensure that if None is returned the value will be Left("error message").right
        // if Some(value) is returned then onErr will extract the value and will return Right(value).right
        firstName <- (xml\"firstName").headOption.map(_.text) orErr("Missing first name!")

        middleName <- (xml\"middleName").headOption.map(_.text) orErr("Missing middle name!")
        lastName <- (xml\"lastName").headOption.map(_.text) orErr("Missing last name!")
        s_age <- (xml\"age").headOption.map(_.text) orErr("Missing age!")
        age <- Try(s_age.toInt).toOption orErr(s"'$s_age' is not a number!")
      } yield
        Person(firstName,middleName,lastName,age)
  }

  // Parse that returns result OR accumulates all errors
  // TODO: actually easier to just implement ParsePattern4 but should do this for illustrative purposes
  object ParsePattern3

  // This parser that provides a log (a monad Writer) inaddition to the result of the parse
  // The log can be used to return error messages for all problems detected
  // Or it can be used to return warnings, info, etc
  object ParsePattern4 {

    // A case class monad Writer that can accumulate log items in addition the parse result
    case class Parse[A](log: List[Any], result: Option[A]) {

      // repr/flatMap that implements the monad Writer pattern
      def map[B](f: Option[A] => Option[B]) = Parse(log,f(result))
      def flatMap[B](f: Option[A] => Parse[B]) = {
        val inner = f(result)
        Parse(log ++ inner.log, inner.result)
      }

      // Syntatic sugar that ensures that if the result is None then the log item is logged otherwise the value
      // is just returned
      def orLog(item: Any) = result match {
        case Some(_) => this
        case None => Parse(log :+ item, result)
      }
    }

    object Parse {
      def empty[A] : Parse[A] = Parse[A](Nil, None)

      // Make a new Parse from an Option with an empty log (if success)
      // If an exception is thrown then add as log item
      def apply[A](opt: => Option[A]) : Parse[A] = {
        try {
          Parse[A](Nil, opt)
        } catch {
          case e : Exception => Parse(e :: Nil ,None)
        }
      }

      // monad Writer pattern - this is used to add an item to the log but don't care about value
      def tell(log: List[Any]) : Parse[Unit] = Parse[Unit](log, None)

      // Many times during parsing we may need to extract a value and then parse it - each step may fail separately
      // This function is used to flatten nested parsing
      def flatten[A](optParse: => Option[Parse[A]]) : Parse[A] =
        optParse match {
          case Some(parse) => parse
          case None => Parse.empty
        }
    }

    // This is a standard functional pattern function that will convert a function that takes any 4 arguments into one
    // that takes an Option for each argument and returns an Option of the return value. If any argument is None the
    // return value will be None
    def lift4[A,B,C,D,E](me: Function4[A,B,C,D,E]) = { (oa: Option[A],ob:Option[B],oc:Option[C],od:Option[D]) =>
      for(a <- oa;b <- ob;c <- oc;d <- od) yield me(a,b,c,d)
    }

    def parsePerson(xml: NodeSeq) : Parse[Person] =
      for {

        // 1) Construct a Parse object
        // 2) Extract firstName from xml and get its text value as above
        // 3) If this fails (or throws an exception) then None is returned
        // 4) orLog will ensure that if None is returned then the message is logged (also the exception if any)
        // 5) As execution flows down the for-comprehension, the Parse repr/flatMap functions will ensure that log items
        // are automatically accumulated and returned in the Parse[Person] result
        // 6) Because Options are extracted from the Parse object, execution will not stop if a value is not present
        // all values will be examined (and all potential error messages accumulated)
        optFirstName <- Parse {
          (xml\"firstName").headOption.map(_.text)
        }.orLog("Missing first name!")

        // Sometimes it is necessary to make decisions based on a parsed value
        // Working with parsed values requires extracting the value out of the Option (if it exists)
        // The Parse.flatten pattern is used when a expression returns a Parse itself
        // Parse.flatten will extract the Option and return it and ensure the logs are accumulated
        // The specific pattern below is used to inject a log item but ignore the result value
        _ <- Parse.flatten {
          for {
            firstName <- optFirstName
          } yield
            // Parse.tell is used to inject a log item - the value it returns is Unit (monad Writer pattern)
            Parse.tell(s"WARNING: It's $firstName!" :: Nil)
        }

        optMiddleName <- Parse {
          (xml\"middleName").headOption.map(_.text)
        }.orLog("Missing middle name!")

        optLastName <- Parse {
          (xml\"lastName").headOption.map(_.text)
        }.orLog("Missing last name!")

        optSAge <- Parse {
          (xml\"age").headOption.map(_.text)
          // Note: we could convert age to integer here, but if we did we would not be able to distinguish a missing age
          // from an invalid age in the log
        }.orLog("Missing age!")

        // Here we need to convert the extracted string to an integer - to work with it must Parse.flatten pattern
        optAge <- Parse.flatten {
          for {
            s_age <- optSAge
          } yield Parse(Some(s_age.toInt)).orLog(s"$s_age is an invalid number!")
        }
      } yield
        lift4(Person.apply)(optFirstName,optMiddleName,optLastName,optAge)

  }

  // Experimental - this was interesting to me but in the end doesn't seem to have any benefits over parse1?
  object ParsePattern5 {
    implicit class pimpMyFunc1[A,B](val me: Function1[A,B]) extends AnyVal {
      def map[C](f: B => C) : Function1[A,C] = { a => f(me(a)) }
      def flatMap[C](f: B => Function1[A,C]) : Function1[A,C] = { a => f(me(a))(a) }
    }

    val parsePerson : NodeSeq => Option[Person] =
      for {
        optFirstName <- { e:NodeSeq => (e \ "firstName").headOption.map(_.text) }
        optMiddleName <- { e:NodeSeq => (e \ "middleName").headOption.map(_.text) }
        optLastName <- { e:NodeSeq => (e \ "lastName").headOption.map(_.text) }
        optSAge <- { e:NodeSeq => (e \ "age").headOption.map(_.text) }
        optAge <- { e:NodeSeq => optSAge.map(s_age => Try(s_age.toInt).toOption).flatten }
      } yield
        ParsePattern4.lift4(Person.apply)(optFirstName,optMiddleName,optLastName,optAge)

  }

  // This pattern breaks parsing into three phases:
  // 1) Extraction: extract strings from input and map to key-value pairs (StringValues)
  // 2) Validation: ensure all values are set and are the correct types and return ValidStringValues[A]
  // 3) Convert: safely convert from ValidStringValues[A] to A
  // Pros:
  // 1) Allows re-use of validation and conversion code by writing different extractors (extract from XML, JSON, CSV,
  // etc)
  // 2) Code could be written to allow for automatically extracting, validating and converting case classes using
  // reflection
  // Cons:
  // 1) Much more verbose
  // 2) Ordering of key-values is discarded and assumed to not be meaningful
  // 3) No mechanism for validation messages to be associated with where they occur in the source
  // 4) StringValues should be probably be expanded
  object ParsePattern6 {

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

  override def main(args: Array[String]) {
    val xml =
      <document>
        <person>
          <firstName>Lance</firstName>
          <middleName>David</middleName>
          <lastName>Gatlin</lastName>
          <age>35</age>
        </person>
      </document> ::
      <document>
        <person>
          <firstName>Lance</firstName>
          <lastName>Gatlin</lastName>
          <age>thirty-five!</age>
        </person>
      </document> ::
      <document>
        <person>
          <firstName>Lance</firstName>
          <middleName>David</middleName>
          <lastName>Gatlin</lastName>
          <age>thirty-five!</age>
        </person>
      </document> ::
        <document>
          <person>
            <firstName>Lance</firstName>
            <lastName>Gatlin</lastName>
          </person>
        </document> ::
      Nil

    xml map(xml => ParsePattern1.parsePerson(xml \ "person")) foreach(println(_))
    println
    xml map(xml => ParsePattern2.parsePerson(xml \ "person")) foreach(println(_))
    println
    xml map(xml => ParsePattern4.parsePerson(xml \ "person")) foreach(println(_))
    println
    xml map(xml => ParsePattern5.parsePerson(xml \ "person")) foreach(println(_))
    println
    import ParsePattern6._ // implicits
    xml map(xml => ParsePattern6.PersonParser.parse(xml \ "person")) foreach(println(_))
    println
  }
}
